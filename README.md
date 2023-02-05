# Gerbil-persist

Gerbil-persist is a package to persist concurrent processes as well as data.
It currently uses [LevelDB](https://github.com/google/leveldb) as a trivial key-value store underneath.
However, we are moving towards abstracting away the underlying store, and
supporting [Sqlite](https://www.sqlite.org/index.html) as default instead on the Gambit C,
and [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) on the Gambit JS.

See [db.ss](db.ss) for what is *currently* the main database interface.

## Copyright and License

Copyright 2020-2023 Mutual Knowledge Systems, Inc. All rights reserved.
Gerbil-Persist is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

## Our Persistence Model

### Persistence

Persistence means that in case the system stops then is restarted,
the user-visible state of the software will survive the interruption
and computation will resume from that state, that was "persisted".

The system may stop for many reasons:
- The computer's power may go down
  (because its laptop batteries are empty, its desktop is being moved,
  the grid experiences interruptions, a man trips on a power cord,
  a disjunctor is tripped, or enemies cut the power).
- The underlying operating system process may be killed
  (because a user closed its window or asked for it to be killed,
  because the operating system ran out of resources and killed it,
  or because some hardware malfunction otherwise occurred).
- One of the internal or external services the system crucially
  depends on may itself be victim of some failure.

Whichever it the cause the system was stopped,
assuming the system state wasn't also corrupted
(at which point nothing more can be done, but which can be prevented
through hardware redundancy, which we will consider as a separate issue),
persistence means that computations will resume from
where the system was at the time it stopped.

### Concurrency

The user can define many concurrent processes that will keep executing
and mutating their state (a.k.a. mutators), while the system will persist
the state of these processes, so that in case the system is stopped,
the processes can be resumed in the state they were in at the time
the system was stopped.

Now while user processes may run concurrently with each other,
the persistence layer itself is single-threaded and
linearizes access to the database. This ensures that the data accesses
from all user threads constitute a consistent state transition
once assembled into a database transaction, such that
processes can be suitably restored from the resulting state.

The linearization constraint could conceivably be relaxed somewhat
by having separate transactions and a clever locking mechanism,
but such elaborate strategies would work neither
on the simple key value stores that we want to support as backends,
nor with the simple validation strategy we intend to use (see below).
If more parallelism is desired, the simple obvious solution is
to use several independent sequential encrypted stores instead of a single one.

(Note that Gerbil compiles to Gambit itself by default compiles to a
monoprocessing execution model. Gambit can also compile to a multiprocessing
execution model, but the Gambit runtime is suspected to still contain
a lot of functions that assume monoprocessing, and that need to be identified
and updated before it is safe to use multiprocessing with Gambit.
Therefore, no actual parallelism is lost with our current strategy,
and running multiple separate Unix processes each with their separate database
is necessary for parallel execution with Gerbil.
On the other hand, Gerbil has a lot of infrastructure for programming with
actors both local or remote, which was actually the original motivation
for writing Gerbil.)

### Restorability Responsibility

The user is responsible for ensuring that the stored data is indeed sufficient
to restore his processes when the machine restarts to a state equivalent
to the one saved at the time just before the machine stopped.

This may be trivial (if the process just passively serve the data), or may
require careful code generation (if the processes implement arbitrary
activities in a Turing-capable language).
Indeed, to represent arbitrary processes, you'd have to materialize the
stack frames of their programs as data structures ---
first-class continuations as marshallable data structures;
that in addition to mapping the storage model of the programming language
to that of the database.

Doing these transformations is tedious and error-prone,
and better done by an automated program and by a human compiler.
Our persistence system does not presently assist with such code generation, but
in the future a virtual machine and a compiler for a Scheme dialect
could be provided that offers a rich enough programming model
while automatically enforcing the persistence invariants.

### Transactionality

A user process can run code in an atomic block to obtain a guarantee that
all changes in the block will take effect in the same database transaction.
The system will either put the process to sleep until it can safely start
the block, or it will let the process run until completion of the block
before it commits anything.
To avoid long pauses, atomic blocks must always be short;
long atomic blocks in one user process may cause the entire system
to experience long delays, deadlocks, and/or memory overflow.

A user process can also wait for its so far scheduled modifications to be
fully committed before it proceeds (possibly right out of an atomic block),
i.e. the equivalent of a Unix `fdatasync()`.
The system will put the process to sleep until after the commit is complete.
Waiting for commits is required before communication of signed commitment
messages to outside parties (e.g. blockchain transactions), but not before
communication to other inside parties within the same persistence environment.
Whether the human at the console is considered "inside" or "outside" in this
context is an interesting topic. Presumably the user interface would display
relevant changes in different colors whether or not they have been fully
committed.

The user may not assume at which point during process execution a commit
may or may not happen, except through its uses of the two facilities above.
The system is then free to choose when to commit changes in order to
maximize time spent doing useful computations versus either sleeping idle
waiting for disk or doing administrative work supporting the useful
computations but not useful in itself.
The system implementer may choose any heuristics, but would presumably pick one
that ensures changes are committed in a timely manner, rather than accumulate
until a giant pause is necessary to commit them all, or worse,
memory runs out, the process is killed, and all is lost before
they had a chance of being committed.

### Transaction Schedule

In practice, the system triggers a database transaction based on the following:
- a timer set some fixed time after the previous transaction was committed, or
- at least one process waiting for data to be committed, and at least
  some time has elapsed since the last transaction was committed.

Indeed, if the system is already waiting for a database transaction to
complete, then there's no point in trying to build a new transaction yet.
Moreover, it doesn't make sense to issue new transaction much faster
than it takes for a transaction to be committed, and so the runtime system
may time how long it took for the previous commit to happen,
and wait for at least as long before it issues a new transaction
--- unless the system is otherwise idle in which case it may proceed.

Nevertheless, if after some further delay, nothing has triggered a commit yet,
and some changes have already been accumulating by the mutators, then
the system will automatically issue a transaction and flush these changes
to the database.
The delay should be somewhat proportional to the time it takes for
a transaction to go through. This would be something like 20ms or so
if the database only writes to a local disk, but could be 500ms or more
if synchronously writing to several database replicas in remote data centers.

### Transaction Waves

On most database backends, the database connection is wholly unavailable
until after the latest prepared transaction is committed anyway.
So any new transaction will have to wait for the previous one to be committed
before reading from the database, much less writing to it.
*If* commits are done in the foreground, all user processes are effectively
stopped until the database transaction is committed.

*If* on the other hand, commits are done in the background,
user processes that are not waiting for commit can keep mutating their state,
but must do so without actually touching the database.
In that case, they could instead be reading from a cache and writing
to that cache, and only block when actual database access is required.
The cache could contain the entirety of the data, or only part of it
at which point attempts to read beyond the cache would block until the
database is available again. Writes to the cache would also be queued
so as to be issued to the database engine once it is available again.
Transactions would then come in overlapping waves, wherein a new wave starts
as the old starts being committed, becomes the only wave once the old one
is committed, and eventually becomes the old wave as it is committed
and a new wave replaces it.
Reading and writing then happen using the current wave, which
either directly accesses the database, or only the cache, depending on
which stage this wave is at.

### Conflicts and Rollbacks

Our persistence system assumes that it has exclusive control on the
underlying database, so that there is no conflict with external writers.
We also assumes that user processes will avoid any conflict between
each other, by using the usual mutual exclusion mechanisms (mutex, etc.).

Our persistence system does not offer any rollback facility:
individual user processes run independently from each other yet partake in
the same database transaction, therefore rollback of one process would
rollback all changes from all processes.
In a way, this is what happens if the entire system is stopped:
all modifications of all user processes are rolled back and
all user processes are reset to the state they were at
as of the latest committed transaction.

If somehow, the application calls for concurrent access to some resources
and rollback of whichever user process tries to access it second,
then the user application must implement its own system of
transactions, conflict-detection and rollback,
on top of what the system provides.

### Encryption Model

Our encryption model is explained in more details in this document: https://mukn.notion.site/Encrypted-Databases-a-Private-Low-Level-Storage-Model-582fd2775289465cb879d6acbfd7ff11

We use row-based encryption, such that the database can be hosted by untrusted
third parties each of whom may make their copy of the data unavailable, but
may neither decode any of the data nor tamper with it.
The only information they can extract is the size distribution of rows in
the database at large and in each transaction (and even that could be reduced
to overall size, at the cost of padding and chunking).

Row-based encryption means that transactions only require incremental sending
of data proportional to the amount of change effected, rather than encrypting
and sending the entire database as with a simple file-level encryption.
Also, unlike naive block-level encryption, row-based encryption allows for
salt to be changed with every update to a row, preventing trivial cryptanalysis
by the storage host XORing the multiple versions of a same block.

The usage model is that the user has a copy of the entire database locally,
but uses remote backups that he doesn't fully trust,
except to keep his data available.
If the user loses his local copy, and/or once in a while
just to verify the integrity of remote copies, the user will download
the entirety of the database from one and/or a collection of his providers,
and check that the root node is indeed the most recent version,
and that the rest of the data is correctly content-addressed from that root.
The root node also includes a checksum of everything
(including the rest of the root node).
All data is encrypted using a symmetric key derived from the user passphrase
used as the seed of a suitable KDF (key derivation function).

(The root node could also include a signature using an asymmetric key derived
from the same seed, but I think this will be redundant and not needed.)

### Low-level Data Representation

Our encryption model allows for content-addressing of pure persistent data
structures plus a number of mutable rows that start with some random salt
that changes at every update. For integrity-preservation purposes,
a hash of the content of all mutable rows is kept in the root node.
The rest of the data is accessed from the root node via the usual
content-addressing mechanism, that has a built-in integrity mechanism.

The integrity checker also verifies that no extra rows are present in the
database: unreferenced rows are deleted by a reference counting
garbage collector, which always work since the content-addressing model
does not allow for cycles in the content-addressing directed graph;
cyclic data structures can still be represented indirectly using integer
indices as explicit pointers into an array. The reference counts themselves
need only be explicitly stored when strictly larger than one, and
can be stored in a separate table: a count of zero is represented
by the row being absent, and a count of one by the row being present but
its reference counting entry being absent from the counting table.
Each index, each user-defined table, uses a hash of its type and name
descriptor as part of its encryption salt, thereby avoiding undesired
collisions, including with the reference counting table itself.

Tables will be represented with B-tree of order 16 (according to Knuth's
definition: maximum number of children in a non-root node), wherein short
entries (7 256-bit words or less) are inlined (if their type allows for
inlining) and larger entries are content-addressed.
If a deterministic encoding is preferred, a patricia tree may be used instead,
there again with or without inlining of leaf nodes.

The top-level entry is stored checksummed and encrypted with a random salt
at a storage key that also serves as checksum of the database master key.
The cleartext stored is a structure containing a timestamp and transaction count
that make it possible to identify which copy is most up-to-date,
content-addressed links to a schema descriptor, a top-level object,
and a table of indexes (including the reference counting table).

### Pre-Commit Hook

Whenever an actual commit is scheduled, a pre-commit hook is run
that may do the following:
- Tell all processes to either roll back or roll forward to a stable state,
  where applicable, then provide all its state as a single merkleized entity.
- Recompute a common content-addressed index of all these processes that
  between commits evolve independently, so that all data may be accessible
  from a single mutable state cell.

## History

This code started as a port from my OCaml library `legilogic_lib`,
refactored, enhanced, and rewritten again.
That OCaml library was already written to support multiple concurrent
activities exchanging messages with blockchain applications,
though the persistence model hasn't been clarified until much later.

In the OCaml variant, every function was defined twice,
as a client half sending a request and some server spaghetti code
processing it, with some data in between and plenty of promises
to communicate (like Gerbil `std/misc/completion`, just separating the
`post!` and `wait!` capabilities). Instead, in Gerbil, I use locking
so functions can be defined only once, at the cost of having to
explicitly use the fields of a struct to share state instead of
just nicely scoped variables.

Also, I added a timer for deferred triggering of batches by transaction.
I thought I had implemented that in OCaml, but that wasn't currently
in the master branch of legicash-facts. This is made necessary in Gambit,
because Gambit is lacking the OCaml feature allowing to run a FFI function
(in this case, the leveldb in parallel with other OCaml functions).
This parallelism in OCaml naturally allowed the workers to synchronize with
the speed of the batch commits, but that won't work on Gambit,
until we debug the Gambit SMP support.

Final difference from OCaml, we use parameters for dynamic binding of the
database context, where OCaml could only use global variables, static binding,
and/or an explicit reader monad.

## Major Refactoring Underway

We are in the process of deeply changing Gerbil-Persist.

In particular we are (1) abstracting away the underlying key-value store, and
(2) [encrypting all data in this underlying store](https://mukn.notion.site/Encrypted-Databases-a-Private-Low-Level-Storage-Model-582fd2775289465cb879d6acbfd7ff11).

### Basically done
- Replace LevelDB by an abstraction over key value stores (see kvs.ss, kvs-leveldb.ss).
- Use sqlite as second backend, intended as default for embedding on Gambit-C (kvs-sqlite.ss).
- Add first layer of encryption: encrypted content-addressed store atop kvs (ecabs.ss).

### Short term planned changes
- Rewrite a kvs-based variant of db.ss handle, queue, and merge multiple transactions (kvs-mux.ss).
- Add second layer of encryption: btrees on top content-addressed store (btree.ss).
- Add third layer of encryption: user-given db schema using gerbil-poo type descriptors (schema.ss).

### Medium term planned changes
- Support [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) on Gambit-JS.
- Implement asynchronous commits and transaction waves with a cache.
- Support synchronous data replication on multiple remote IPFS providers.

### Long term unplanned hopes
- Support [PostgreSQL](https://www.postgresql.org/) on Gambit-C.
- Support [CockroachDB](https://www.cockroachlabs.com/) as a replicated key-value store,
- Implement our own shared-memory object database in the style of
  [manardb](https://github.com/danlentz/manardb).
