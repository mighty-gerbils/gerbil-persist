# Storage Domains

A **storage domain** is a fully self-describing, persistent storage entity. It can
store data in relational tables, a key-value store, or a page store. It also carries
standardized metadata enabling long-term management: encryption and privacy policy,
redundancy across identified backends, live executor state, and the information needed
to recover, migrate, or hand off the storage domain to a new executor or infrastructure.

Storage domains are the unit of storage policy. Every application is bound to a storage
domain by its controlling meta-object and transparently inherits all of its policies.

---

NB: A large part of this document is slop produced by Claude under my supervision.
https://claude.ai/share/867e1873-f77b-4905-ae0f-af87930a7f20


## 1. Security and Isolation

Every application is bound to an execution domain by its controlling meta-object, and
transparently inherits all of the execution domain's policies, including its storage
domain. A regular application is not allowed to know what execution domain it is bound
to, let alone its storage domain. Application code that attempts to discover this
information is automatically flagged as malicious.

Only the owning execution domain may write to a storage domain. Writes from any other
execution domain are treated as a bug or attack.

Typically, only the domain manager is allowed access to storage domain configuration.
By default the domain manager enforces the following policies:

- It will refuse to **export** its data into any execution domain allowed to communicate
  with the outside world. It will only export it to a management console for the administrator.
- It will accept data **imported** from such a domain as change proposals, but will only
  apply proposed changes after analyzing their consequences, and requiring confirmation
  from the administrator for each suspicious item detected.
- The analysis itself happens in a temporary execution domain that is only allowed to
  produce a report. Analyst software may have its own metadata anonymized during
  analysis, and deanonymized afterward, so the analyst cannot leak what it is analyzing
  or make treacherous decisions while still producing a report that can be correlated
  back to real items by the domain manager.

These are default policies. The domain manager's own policies are configurable by
whoever controls it, within constraints set by the level above.

### Policy Analyzer

A policy analyzer checks storage domain configurations for security issues. It warns
or errors when:

- A storage domain leaves some data in clear text, yet handles private data.
- Some proposed backends are on a blacklist, or fail to be on a whitelist.
- The estimated cost of the change involves budget overruns.

The policy analyzer runs at storage domain definition time and again when
configurations are upgraded. Its findings are presented to the domain manager for
confirmation before any change is applied.

---

## 2. Authentication and Encryption

### Backends Are Dumb Byte Stores

Backends store and retrieve bytes. They have no knowledge of encryption,
authentication, or what the bytes mean. Authentication and encryption are applied
exclusively by the storage strategy — not by the backend. A backend with no storage
strategy wrapping it stores plaintext with no authentication whatsoever.

This is a legitimate configuration in many cases:

- **Development and debugging** — cleartext storage is far easier to inspect, debug,
  and reason about. Inspecting a raw SQLite file or S3 object directly is valuable
  during development.
- **Already-encrypted infrastructure** — if the underlying infrastructure provides
  encryption (encrypted disk, encrypted VPN, trusted private cloud), adding a storage
  strategy on top is defense-in-depth rather than a necessity.
- **Fully trusted internal infrastructure** — if the operator controls and trusts all
  access paths, application-level encryption may be unnecessary overhead.
- **Performance-critical paths** — encryption and authentication have real costs;
  some workloads may not justify them.

The policy analyzer flags configurations with no storage strategy and requires the
operator to explicitly acknowledge the implications. It does not block such
configurations. The operator may have good reasons, and the system respects that.

When a storage strategy is present, encryption is applied **once**, at the physical
storage layer, by the strategy. The logical layer always operates on plaintext. There
is no redundant application-level encryption — if the underlying infrastructure
already encrypts, the storage strategy's encryption is additional protection, not
duplication at the logical level.

### Threat Models

The storage domain's encryption and authentication mechanisms defend
against adversaries who can read backend storage contents
but cannot access the executor's process memory.
Two deployment contexts arise naturally, with different adversary profiles:

**Remote backends** — the adversary is the storage provider,
or anyone who can read the remote backend's contents:
a rogue employee, a subpoenaed provider,
a hacker who breached the provider's infrastructure.
They see only an append-only sequence of encrypted, padded, authenticated changeset blobs.
They cannot observe individual object sizes, access patterns, schema structure, or logical content.
They can possibly modify the data, but they cannot break encryption or authentication,
thus any tampering of the data or meta-data will be evident.
They can observe the volume and timing of writes (how many blobs, how often),
which can be obscured further with the `noise` option.

**Local backends** — the adversary is a confused deputy or
sandboxed code running in a container with visibility into local storage,
but without access to the executor's process memory or master key.
They see the same encrypted changeset blobs as a remote adversary would —
the local backend stores the same opaque authenticated blobs regardless of
whether it is SQLite, a local directory, or an in-memory store.
As long as the master key has not leaked from process memory, the local adversary
learns nothing from storage contents or access patterns
beyond write volume and timing, which can again be obscured with `noise`.

Both contexts are addressed by the same unified changeset-based storage model.
The `backed-storage` combinator handles the local/remote deployment asymmetry —
`front:` for fast local reads, `backup:` for durable remote redundancy —
but both sides store the same encrypted changeset blobs.

**Trusted Deployments** — In some cases, the operator trusts the entire storage path,
or is in a development or debugging context.
Data is stored as plaintext with no authentication.
The policy analyzer requires explicit acknowledgment.
This is appropriate for development, already-encrypted infrastructure,
or fully trusted internal deployments.

### The Master Key

The root of trust for a storage domain is its **master key**. All domain-specific
keys, slot names, and salts are derived from it. The master key may be:

- Derived from a **seed** (passphrase, random bytes) via PBKDF2
- Held in a **hardware key store** (TPM, HSM), identified by fingerprint
- Derived from a **parent master key** via HMAC, forming a key derivation tree

The preferred way to reference a key is by its **fingerprint** — the vault resolves
the actual key material from the fingerprint, whether that means looking in a TPM,
an HSM, a password store, or deriving from a parent key. Hardware key stores advertise
their keys' fingerprints directly.

The key derivation tree allows a single root key to govern a hierarchy of storage
domains and subdomains. The tree can be audited by publishing derivation records
(see §2.4).

### Key Derivation

All derived values are computed via PBKDF2 and HMAC:

```scheme
(define (backend-key uri) (hmac domain-key "backend-key:" uri))
(define domain-key-fingerprint (sha256 domain-key))
```

Each backend has its own derived key, used by storage strategies for authentication
and encryption. When no storage strategy is present, these derived keys are unused —
data is stored as plaintext. Compromising one backend's key does not compromise any
other backend, even within the same storage domain.

All derived slot and table names are indistinguishable from random bytes to an
adversary without the master key, regardless of whether a storage strategy is in use.
Two storage domains sharing a backend have completely disjoint namespaces even if they
use the same logical table names, even in cleartext configurations.

NB: on some backends, domain state may be stored as meta-data of the configuration object?
(NB: Don't forget authentication on that meta-data, too, with:
`nonce||E(M,data)||H(M||backend||max-changeset||nonce||E(M,data))` or such)
Or if atomic metadata is not available, a configuration object may contain
two numbered configurations, and the status then tells us which number is active?

### Key Fingerprint and Derivation Records

A key's fingerprint is a public identifier that uniquely identifies the key without revealing it,
typically `(sha256 key)` or some other such hash. Hardware and software key stores advertise their
keys' fingerprints directly, enabling vault lookup by fingerprint.
A software vault may keep also keep derivation records of how to have a hardware key store
(TPM, HSM, Yubikey) re-derive a key from its seed by a series of HMACs:
```
(hmac (hmac (tpm-key "deadbeef...") "level-1") "level-2")
```

A domain configuration may similarly specify a key to be used for some backend or sub-domain as
a path `("level-1" "level-2")` from key identified by some initial fingerprint, yielding some
expected final fingerprint. The final fingerprint serves as checksum that everything went as expected.

---

## 3. Bootstrap, Recovery, and Survival

### Bootstrap Data

The operator must remember or store:

```scheme
(define BootstrapData
  (Record
    domain-key:     KeySpec        ; fingerprint and/or hmac path from it to get to the domain key
    backend-uri:    URI            ; any one known live backend; need not be secret
    [backend-type:  BackendType])) ; 's3 'gcs 'ipfs 'sqlite 'webdav, etc., if not auto-detected.
```

You definitely need to remember or somehow safely backup the 24-word or so BIP39 seed
from which to recover the key and all the data that follows from it, and
any HMAC path you used to name the domain.
On the other, you don’t have to memorize the `backend-uri`,
since it can be retrieved from your storage provider's console:
they bill you, they better let you access the data you pay for.
Of course, it's much easier if you write all down all the information,
send yourself an email with the entire storage configuration,
or save it in a local file with a remote backup, etc.
Nevertheless the seed is the irreplaceable piece you must keep safe —
everything else can be reconstructed given sufficient access to account records.

### Survival Guarantees

**Configuration survives** if the operator retains bootstrap data AND a quorum of
backends is live and consistent.

**Data survives** if the storage quorum is satisfied by surviving or restored backends.
No protocol trick substitutes for a satisfied quorum.

These are independent.

### The Domain State Cell

The single fixed-address bootstrap pivot point. Every backend has this cell at a
derived address:

```scheme
(define DomainState
  (Record
    sequence:        SequenceNo         ; fencing token — covers entire database
    metadata:        (Maybe ChangesetLocation) ; hint to find other metadata, if using LSM strategy.

    ;; executor lease
    executor:        (Maybe ExecutorID)
    lease-deadline:  (Maybe Timestamp)

    ;; upgrade / migration state
    root:            (Maybe DBAddress) ; root for the current state
    migration:       (Maybe MigrationProgress))) ; status of any ongoing migration

(define ChangesetLocation
  (Record
    changeset: SequenceNo
    offset:    Nat))

(define MigrationProgress
  (Record
    phase:      MigrationPhase
    fraction:   Rational
    checkpoint: Ref))

(define MigrationPhase
  (Enum Prepare BulkTransfer Verify Commit Cleanup))

(define SequenceNo    Nat)
(define ExecutorID    Opaque)
(define Timestamp     Opaque)  ; comparable; resolution backend-specific
```

The `sequence` field fences the entire database — every transaction increments it.
A stale executor cannot update any part of the record once it has lost the lease.
The `peer-backends` list contains enough backend URIs to attempt quorum verification
before the full configuration is readable. It is kept in sync with the full backend
list during configuration upgrades.

### The Configuration Blob

The configuration is at a fixed location, `domain-configuration-location`.
If migrating to a new configuration, it might be a `MigratingConfiguration` instead.
A configuration contains all storage and execution policies:

```scheme
(define DomainConfiguration
  (Record
    ;; storage domain policy
    storage:         StorageExpr
    schema:          Ref
    recovery:        RecoveryPolicy
    backend-health:  BackendHealthPolicy

    ;; execution domain policy
    flush:           FlushPolicy
    barrier-defaults: BarrierDefaults
    scheduler:       SchedulerPolicy

    ;; forward pointer if retiring
    retired:         (Maybe Ref)))

(define MigratingDomainConfiguration
  (Record
    current:  DomainConfiguration
    next:  DomainConfiguration))
```

The domain configuration is always available in full even when the current backend
is but a shard of a RAID-style quorum, out of synch with other shards.
It should be fairly short anyway. It is stored salted, encrypted, authenticated.
It changes only when upgraded, in the usual transactional way.
Global counter and writing a new immutable configuration blob at a new location.


### Bootstrap and Chain-Following Procedure

The bootstrap backend may be stale, failed, or from an old configuration. The
procedure follows a chain of domain state pointers until it reaches the current
authoritative configuration:

```
1.  Resolve domain-key from fingerprint via vault

2.  Contact bootstrap backend
      → read domain-state-slot
      → get sequence, config-location, peer-backends, retired, next-config

3.  Loop in parallel over increasing set of followed backends, until all are processed:

      a. Connect to the backend, try to read its latest state.

      b. If the backend is accessible, has valid state, and is marked as active,
         grab its executor lock (OK to remove lock from dead former self, not from others).
         If could not become the executor, abort the entire thing,
         attempt to release all executor locks grabbed so far,
         wait with exponential backoff to avoid further collision.
         If could become the executor, read the state again again to be sure.

      c. If accessible and valid (active or not, but executor if active):
         Remember the latest sequence number reached,
         read and remember the configuration, and
         add any other backend listed as active to the follow-set
         (if the backend is retired, they are those listed as replacement).

      d. If inaccessible, accessible but with invalid content, or accessible and inactive
         (retired, or not yet active due to incomplete migration),
         mark with appropriate failure.

4.  Identify those active backends with the highest valid sequence number (if any active left),
    adopt their state and configuration (error if disagreement).
      → now you have the latest domain configuration (no consensual state yet, though)

5.  Contact quorum of backends in the domain configuration to identify latest consensual state.
      → error if no consensus found with suitable quorum

6.  Restore database from latest consensual state
      → error if inconsistency found doing so.

7.  If some backends have a valid changesets newer than the consensus,
    possibly conflicting ones, pick one of those proposed (lists of) changesets,
    and attempt to replay them on all backends,
    possibly marking backends with conflicting changesets as inactive pending repair.

8.  Initialize executor state from latest state reached.

9.  Resume execution
```

The loop terminates because there is finite max sequence number that was written,
and it involves a finite number of backends.

---

## 4. Executor Model

### Single Changeset

The execution domain has a single active executor, that will write to the storage domain.
All threads of the executor (if multithreaded) partake in a single current open changeset,
where uncommitted writes from all threads are accumulated.
There are no concurrent writes to the store, writes are centralized by the storage manager.

When the changeset reaches a certain size, or a timeout occurs since it was first written to,
or some thread with the administrative capability requests it, the changeset is closed
(no new addition allowed) and scheduled to be written to all database backends.
A new changeset is open and made current (or will be next time a write is made).

### Yes Optimism, No Speculation

A thread keeps executing optimistically: all writes from a thread are locally
visible immediately by all other threads, even though they are not yet committed.

All changes are definitive and irreversible.
There is no speculative execution, no “rollback”, unless it is done by hand the hard way.

### Critical Sections

Threads must use the usual critical section mechanisms to ensure
there are no race conditions, no conflicts, in accessing memory.

In particular, if for some reason they want to prevent the current changeset
from being closed in the middle of some atomic operation, they can start
a critical section with a read lock on a resource for which the changeset closing
operation needs a write lock while they complete their operation.

Here are some examples of modifications that require a critical section to ensure that
they are part of the same changeset as some update:
  - updates to user-maintained index tables, views, etc., not expressible in some underlying backends
  - updates to maintain accounting on how much of which changeset is still live,
    for the sake of garbage collection of old changesets.
  - updates to enforce some schema constraint in general.

### Memory Barriers

At times, program needs to know that data has been committed before to proceed;
for instance, ensuring an order has been safely recorded before to send
a confirmation to a user, or an item return safely recorded before to send a refund for it, etc.
It then inserts a memory barrier between the recording of the critical event,
and the execution of a response to it.

When a thread hits a memory barrier (execute the corresponding instrutions),
the memory barrier stops the thread execution until the current changeset
has been closed then successfully committed to the database
(i.e. accepted by a quorum of the backends).
If the thread has the required capability,
the memory barrier may optionally have an urgency,
an amount of time after which it forces the closing of the current changeset;
with sufficient capabilities, thread may use the same mechanism to track
not just which changes are fully committed, but where in its lifecycle is
the changeset containing the change, so that the UI may notify the user
of ongoing progress.

### The Changeset Lifecycle

Multiple changesets may be in flight simultaneously:

```
changeset N-1:  fully committed on all backends
changeset N:    quorum-committed — durable, still propagating to remaining backends
changeset N+1:  closed, submitted to remote, awaiting quorum acknowledgment
changeset N+2:  open, current pending writes, not yet submitted
```

As backends acknowledge changesets, the execution domain advances threads
whose memory barrier conditions are met.
A thread waiting with low-urgency may have its writes committed
as a side effect of another thread's lower-latency barrier — natural batching with no extra cost.

### Crash Recovery and Re-execution

After a crash, execution resumes from the latest quorum-committed changeset.
Anything beyond that — submitted but not quorum-committed, or still in the pending changeset
— is either successfully replayed or discarded.

Re-execution must be deterministic from the logged changesets.

---

## 5. Configuration and Data Upgrade Protocol

Configuration and data upgrades are always coupled. All upgrade state is tracked in
the domain state cell. The protocol is crash-safe at each phase boundary.

### Phases

  1. Ensure new backends exist, are accessible.
     Create or update their status as being under repair by current executor.

  2. Write new Configuration blob to metadata.
     Wait for commit.

  3. Treat all new backends as being part of the set of backends undergoing repair:
     First delete any object not supported by our encoding,
     and any changeset the hash or size of which contradicts our history.
     Then copy all the changesets from existing backends, with hash as metadata
     (or, on S3, enable SHA-256 option on upload if that's our hash).
     A backend shall remain "under repair" even when it is all synch'ed up,
     so it keeps getting new changesets, and deleting obsolete changesets.
     Obviously, it is not part of a quorum yet.
     Push changesets to old/active backends first, new/under-repair backends second.

  4. When all new backends are ready, write a changeset that updates the configuration,
     close and commit it immediately. Push changeset to old backends first, new backends second.
     Locally mark as retiring those backends no longer part of the configuration;
     stop sending them further changesets.

  5. As backends confirm receipt of the changeset that switches configuration, change their status:
     Retired backends update their status to retired, with new configuration as next-of-kin.
     New backends change their status to active with new configuration.
     Backends that are still active in both configuration remain active,
     but update their configuration.

  6. Once a quorum of old backends and new backends are both updated, the switch is complete.
     Notify the administrator of his success.

  7. Optional cleanup: after some time chosen by the administrator to check that
     the new configuration is working
     (not too fast, to make sure that the new setup works fine),
     for each retired backend, at the administrator's preference:
     delete the backend altogether, or delete its changeset data, or archive it, etc., to save money.

### Crash Safety

TODO: Verify that the algorithm is safe.

---

## 6. Backend Health and Failure Handling

When something bad happens to a backend, or it falls behind in accepted changesets,
it is added to the failure list, its status is changed to "under repair",
and the repair process happens as in step 3 or paragraph 5 above.
When the backend catches up, it changes its status to active again.

Backend failure detection and repair are executor concerns — they do not require a
storage configuration upgrade to change.

---

## 7. Storage Configuration

The storage configuration is a tree expression declared as the `storage:` field of
`DomainConfiguration`. The tree composes backends (leaves), quorum combinators, and the
`backed-storage` combinator. The changeset encoding, encryption, compression, and
padding are domain-level concerns — the storage tree is purely about topology.

### Backend Expressions

```scheme
(s3-backend      uri [credentials: c])
(gcs-backend     uri [credentials: c])
(ipfs-backend    uri [credentials: c])
(sqlite-backend  uri [credentials: c])
(file-backend    uri                 )
(rsync-backend   uri                 )
(webdav-backend  uri [credentials: c])
```

Credential lookup is implicit by default — the vault is queried by URI (and user if
specified). The `credentials:` keyword supplies an explicit key reference:

```scheme
(s3-backend "https://my-bucket.s3.us-east-1.amazonaws.com/myapp/")
(s3-backend "https://my-bucket.s3.us-east-1.amazonaws.com/myapp/" credentials: "myuser")
(s3-backend "https://my-bucket.s3.us-east-1.amazonaws.com/myapp/"
  credentials: (key-ref "a3f8e1..."))
```

Backend types are open-ended. Suitable multi-methods are defined on each backend
class to handle its specific behavior and capability profile.

### Quorum Combinator

```scheme
(quorum n storage-expr ...)
```

`n` is the retrieval threshold.
`(quorum 1 b1 b2 b3)` — any one suffices (mirror).
`(quorum 2 b1 b2 b3)` — tolerates one failure out of three backends.
`(quorum n b1...bn)` where n equals list length — plain sharding, no redundancy.

Writes always go to all backends regardless of threshold. The threshold governs read
availability only. Quorums nest freely for RAID-of-RAID structures.

### Backed Storage

Combines a front storage, typically local, for actual reads and writes, and a
write-only backup storage, typically remote, for robustness. The backup is only
read from in case of a crash or loss of the front storage.

When activating backed storage, both front and backup are activated separately and
reconciled:

- If front is behind backup: error, with the option of restoring front from backup.
- If front is ahead of backup: roll forward the backup, then resume.
- If in sync: resume immediately.

The front additionally stores, alongside regular data:

- For each object, in which changeset it was last modified.
- For each changeset, how much live data it contains.

This metadata supports GC scheduling — identifying which changesets are mostly dead
and candidates for recycling — without needing to scan the backup.

```scheme
(backed-storage
  front:  storage-expr     ; local — typically a single sqlite or file backend
  backup: storage-expr)    ; remote — typically a quorum of backends
```

### Changeset Size

Two domain-level parameters bound changeset size:

```scheme
changeset-padding:  n    ; pad each closed changeset to the next multiple of n bytes
                         ; hides exact write volume; default 4096
changeset-max-size: n    ; hard upper bound on changeset size; default 2097152 (2MB)
                         ; changeset is closed and flushed when this is reached
                         ; regardless of execution domain flush policy
```

Between these bounds, when a changeset is closed is an execution domain concern.

### Compaction

Old changesets accumulate dead objects — objects whose values have been superseded
by later writes. Compaction recycles old changesets by copying their surviving
objects into fresh changesets and deleting the originals.

The compaction policy is a domain-level parameter:

```scheme
(define compaction-expr
  (OneOf
    (tiered-compaction
      [threshold: r]    ; recycle changesets when live fraction drops below r; default 0.5
      [levels: n])))    ; number of tiers; default 4
```

Tiered compaction is the default — appropriate for write-mostly workloads where
reads occur primarily on crash recovery or migration. Compaction runs as a background
process using idle write budget, spreading GC work across normal commits without
generating extra requests to backends.

A few potential optimizations related to compaction:
  - Since PUTs are expensive on S3, you can save money by merging lots of old changesets
    into a single one... which is conceptually the same as saving an image!
  - Other slight optimization: if there is no memory barrier,
    you can save a changeset now for immediate survival, then instead of forgetting it,
    have the next changeset include its changes, and obsolete it, etc.,
    until a memory barrier is hit and then you really must persist remotely.

---

## 8. Schema Metadata

The schema is an encrypted blob stored as part of the `Configuration`. Its content —
table definitions, column types, reference constraints, — is specified in a separate document.
It changes only on a data format migration,
producing a new `Configuration` blob at a new changeset location.

---

## 9. Domain DSL — Grammar Summary

```scheme
;; Top-level storage domain value
(storage-domain
  domain-key:         key-expr              ; required
  changeset-padding:  n                     ; default 32768
  changeset-max-size: n                     ; default 2097152 (2MB)
  compression:        algo                  ; default zstd
  noise:              noise-expr            ; default none
  compaction:         compaction-expr       ; default (tiered-compaction)
  storage:            storage-expr
  schema:             blob-or-ref)

;; Storage tree
(define storage-expr
  (OneOf
    backend-expr
    (quorum n storage-expr ...)
    (backed-storage
      front:  storage-expr
      backup: storage-expr)))

;; Backends
(define backend-expr
  (OneOf
    (s3-backend      uri [credentials: c])
    (gcs-backend     uri [credentials: c])
    (ipfs-backend    uri [credentials: c])
    (sqlite-backend  uri [credentials: c])
    (file-backend    uri                 )
    (rsync-backend   uri                 )
    (webdav-backend  uri [credentials: c])
    ...))

;; Compaction
(define compaction-expr
  (OneOf
    (tiered-compaction
      [threshold: r]                        ; default 0.5
      [levels: n])))                        ; default 4

;; Key expressions — two contexts with different permitted forms

;; Bootstrap context: used to obtain the domain key before configuration is read.
;; These forms are never stored in configuration.
(define bootstrap-key-expr
  (OneOf
    fingerprint-string                      ; sugar for (key-ref fingerprint)
    (key-ref   fingerprint)                 ; vault resolves by fingerprint
    (seed->key passphrase)                  ; pbkdf2 from seed — never stored
    (tpm-key   identifier)                  ; on-board TPM
    (hsm-key   identifier)                  ; external HSM
    (yubikey   identifier)))                ; YubiKey / smart card

;; Configuration context: used inside stored configuration, after domain key is
;; resolved. Never contains seed or hardware device references.
(define config-key-expr
  (OneOf
    fingerprint-string                      ; sugar for (key-ref fingerprint)
    (key-ref   fingerprint)                 ; reference a key other than domain key
    (subkey    config-key-expr label ...
               [fingerprint: hash])))       ; verify derived key matches

;; Noise
(define noise-expr
  (OneOf
    (constant-noise  kb-per-second: r)
    (adaptive-noise  kb-per-second: r)
    (scheduled-noise schedule: s)))
```

---

## 10. Example Storage Domain Definitions

```scheme
;; Simple local storage domain — no encryption, trusted storage
;; (policy analyzer flags if used with non-public data)
;; domain-key is supplied at bootstrap time (e.g. from seed or yubikey)
;; and is not stored in the configuration itself
(define local-dev
  (storage-domain
    domain-key: "local-dev-fingerprint"     ; resolved by vault at bootstrap time
    storage:    (sqlite-backend "file:///~/.myapp/dev.db")
    schema:     my-schema))

;; Browser-based storage domain
;; remote is durable encrypted backup
;; system remains available if either local or remote is down
(define browser-domain
  (storage-domain
    domain-key: "a3f8e1..."
    noise:      (adaptive-noise kb-per-second: 5)
    storage:
      (backed-storage
        front:  (sqlite-backend "file:///~/.myapp/store.db")
        backup: (quorum 2                   ; any 2 of 3 — tolerates one failure
                  (s3-backend   "https://my-bucket.s3.us-east-1.amazonaws.com/myapp/")
                  (gcs-backend  "https://storage.googleapis.com/my-bucket/myapp/")
                  (ipfs-backend "https://ipfs.infura.io/myapp/")))
    schema: my-schema))

;; Private server storage domain
;; local sqlite for fast reads; encrypted remote backup with quorum 3/4
(define production
  (storage-domain
    domain-key: "b7c3f2..."
    storage:
      (backed-storage
        front:  (sqlite-backend "file:///var/myapp/store.db")
        backup: (quorum 3                   ; any 3 of 4 — tolerates one failure
                  (s3-backend   "https://my-bucket.s3.us-east-1.amazonaws.com/myapp/")
                  (s3-backend   "https://my-bucket.s3.eu-west-1.amazonaws.com/myapp/")
                  (gcs-backend  "https://storage.googleapis.com/my-bucket/myapp/")
                  (ipfs-backend "https://ipfs.infura.io/myapp/")))
    schema: my-schema))

;; High-redundancy storage domain — RAID of RAID
(define high-redundancy
  (storage-domain
    domain-key: (subkey "org-root-fingerprint" "high-redundancy")
    storage:
      (backed-storage
        front:  (sqlite-backend "file:///var/myapp/store.db")
        backup: (quorum 1                   ; either group suffices
                  (quorum 2                 ; any 2 of 3 in group A
                    (s3-backend "https://my-bucket.s3.us-east-1.amazonaws.com/myapp/")
                    (s3-backend "https://my-bucket.s3.eu-west-1.amazonaws.com/myapp/")
                    (s3-backend "https://my-bucket.s3.ap-east-1.amazonaws.com/myapp/"))
                  (quorum 2                 ; any 2 of 3 in group B
                    (gcs-backend  "https://storage.googleapis.com/my-bucket-us/myapp/")
                    (gcs-backend  "https://storage.googleapis.com/my-bucket-eu/myapp/")
                    (ipfs-backend "https://ipfs.infura.io/myapp/"))))
    schema: my-schema))

;; Subdomain — domain key derived from parent storage domain's key
(define sub-domain
  (storage-domain
    domain-key: (subkey "parent-fingerprint" "sub-domain")
    storage:
      (backed-storage
        front:  (sqlite-backend "file:///var/myapp/sub.db")
        backup: (quorum 2
                  (s3-backend   "https://my-bucket.s3.us-east-1.amazonaws.com/sub/")
                  (gcs-backend  "https://storage.googleapis.com/my-bucket/sub/")
                  (ipfs-backend "https://ipfs.infura.io/sub/")))
    schema: my-schema))
```
