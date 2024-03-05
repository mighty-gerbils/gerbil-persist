# Persistent Concurrent Activities

_Orthogonal Persistence_: You bind variable x to 42, but then some "youths"
steal your laptop and orcs raze your datacenter to the ground. So you move to a
safer place and enter your passphrase in a new laptop, the system downloads
backup copies from redundant datacenters on multiple other continents, and
variable x is still bound to 42, yet not one single instructions in any of your
applications ever had to even mention anything about storage and retrieval.

## Orthogonal Persistence

### Persistence by Default

Orthogonal Persistence (of Processes, or however named activities),
that John de Goes calls “Durable Execution”, is when
ongoing processes persist (at least by default),
such that if interrupted for any reason (memory exhaustion,
hardware fault, power interruption, machine destroyed or stolen, etc.),
they will automatically restart from last committed state.

Orthogonal Persistence is to be opposed to the prevalent paradigm of
Manual Persistence, wherein all data is transient by default, and
developers have to explicitly write data to one or several files on disk
when they desire persistence, and further organize their actions into
database transactions that they commit when they desire atomicity.
Orthogonal Persistence essentially reverses the burden of persistence:
all state changes are safely written to persistent storage by default,
and escaping into transience is the edge case sometimes invoked for performance.

Although most issues with Manual Persistence disappear with
Orthogonal Persistence, some issues remain, though in a simplified way,
reframed upside down into not having been Persistence issues after all,
so much as essential logical issues that had been previously conflated
with the cumbersome details of their manual persistence:
*persistence domains*, *atomicity*, *synchronization*,
*publishing*, *resource exhaustion* and *schema upgrade*.

### Persistence Domains

Orthogonal Persistence can use traditional disks, filesystems and databases
as underlying storage as well as cloud storage.
Each (partition of a) disk, (registered directory tree in a) filesystem,
(set of tables in a) database or (prefixed area within a) cloud account
is presented as a “persistence domain” to the user
who wants to configure where and how his data will be persisted.

In the Manual Persistence paradigm, developers talk about
“Storage Backends”, and focus on physical devices and
the many layers of drivers to access them, and
how they are statically assembling them up into larger storage “volumes”.
In the Orthogonal Persistence paradigm, developers prefer to talk about
Persistence Domains as logical entities having a persistent identity,
and how they are dynamically mapped down to various underlying substrates.
This means a persistence domain can survive indefinitely
through simple occasional reconfiguration, even though the
underlying substrates necessarily have short finite shelf life
of a few years at most each, which necessarily causes catastrophic
transitions in the Manual Persistence paradigm.

When mapping a persistence domain to a database or cloud service,
transactions would of course be used for the sake of atomicity.
On a filesystem or a raw device, atomicity would be implemented by
batches of append-only writes followed by a `fdatasync`,
then possibly a head-switching and another `fdatasync`,
if the previous isn’t enough (or possibly as part of the next transaction,
or with a protocol relying on multiple potential heads and checksums
so the restore procedure can locate the latest head).
To let the storage backend see the mutator actions in terms of transactions
without the mutator being explicitly organized in such terms,
Orthogonal Persistence uses *inversion of control*, whereby
it maintains at any point in time one transaction per persistence domain.

For the sake of liveness, the system can periodically
commit these transactions and starts new transactions.
For the sake of durability, replication can be achieved by federating
several persistence domains using some private consensus mechanism.
For the sake of privacy, encryption can be used.
For the sake of integrity, cryptographic digests can be used.

### Atomicity

Atomicity is the notion that some speculative effects must happen all together,
or none at all must happen, and that users should never be able to observe
the system in an “intermediate” state where some happened but not the others.
For instance, accounting books must be balanced at all times, and
when a transfer is attempted, it may fail or complete, but users should never
observe accounts in a state where one was credited and the other not
debited (yet), or the other way around.

With respect to Persistence, the speculative aspect is about whether or not
some computation was persisted or not. If it was persisted, then it is
guaranteed to have taken place even in the case of the system crashing and
restarting; if it wasn’t, then it is possible for the system to forget it.
The crash may be fortuitous and unexpected (sudden power loss, theft of device),
undesired but expected (laptop with empty battery, device failing of old age),
or caused by Enemy action (random attack by a predator, or targeted attack by
a personal or professional adversary). In any case, the system is supposed to
restart in a state consistent with the effects either having happened or not,
but not in a garbled state where some of them happened but not others, or out
of order, or with some mixups in the details, or data corruption, or with
the entire system in an unrecoverable state, etc.

In the paradigm of Orthogonal Persistence, atomicity is managed with explicit
_atomic sections_: code that produces intermediate states that should not be
persisted is wrapped in such sections, quite similar to
code sections that have disabled signals or interrupts,
or that hold some mutual exclusion lock.

Atomic sections must be short: long atomic sections will break liveness,
i.e. may cause the system to become unresponsive.
But there is otherwise no problem with nesting atomic sections:
persistence remains disabled while executing within an atomic section,
and is only reenabled when all nested atomic sections have been exited.
Code that contains atomic sections can be freely composed with other
code containing atomic sections, and you do not usually need even be aware
of whether or not the code components you compose contain
one or more atomic sections, or none at all.

Based on disk latency, we may target say a millisecond as duration
before which to commit the current transaction.
When the timer is reached, the transaction is delayed
until all current atomic sections are completed;
and (possibly after a grace period) new atomic sections are blocked
from even being started, until after the transaction is committed.

Atomic sections can be contrasted with transactions,
the way that atomicity is managed in manual persistence paradigm.
With manual persistence, you often get no guarantee of atomicity:
you have to hope nothing bad happens when the power eventually goes down.
If you really care about atomicity, you use a database and organize
your code into transactions within which you explicitly put the data changes
that should be persisted.
But unlike atomic sections, transactions are
a low-level yet global concept that isn’t modular.
See section below on *Modularity*.

### Synchronization

Synchronization is the ability to wait for some data to have been
persisted for sure before to take action that depends on it
(such as sending ensuring a transfer was complete before sending
acknowledgements, or signing a follow-up transaction, etc.).

In the Orthogonal Persistence paradigm, this is managed using explicit
_write barriers_: an instruction or procedure that pauses evaluation
until after the changes so far are committed. Optionally, it may be
combined with thread-spawning to register a callback function to be called
after the current transaction commits, all from within an atomic section.
Note that evaluation after a write barrier can actually continue optimistically
after the write barrier, as long as the side-effects it produces are not
observable by users unless and until after the changes are committed.

By contrast, with manual persistence, synchronization requires
one to explicit `COMMIT` a transaction (on a database), or
to explicitly call `sync`, `fsync` or `fdatasync` (on a filesystem),
in an operation that is outside of the transaction itself,
typically running on a different machine, the client,
rather than on the server.
The client may try to make a complex computation made of several
transactions, called a “saga”, but by definition, anything
that happens outside the database is itself a process that doesn’t persist,
that may somehow be restarted thanks to complex and onerous infrastructure
on top of the operating system, but will have lost all its data and context.
The client is furthermore written in a programming language that is
completely different from that of the database, with its own evaluation model,
which programmers usually prefer because programming languages that come
with databases comparatively suck in too many ways to count.
Moreover, all computation in a client thread is usually stopped
while waiting for the transaction to be persisted, which adds a lot
of latency and slowness to anything that uses the database,
that can only be overcome by heroic efforts from developers.

Synchronization and Atomicity go together: in Computer Science,
every set of constructors or introduction rules come with
a corresponding set of destructors or elimination rules.
In other words, a concept matters only if you can make observations from it.
Atomicity is about the grouping of some speculative effects together;
synchronization is how you ensure that the speculation is settled…
before you start new atomic groups of effects.

### Publishing

Publishing is what users do when the changes they made have reached
a stable, satisfactory point, or when they otherwise want to make
a test branch or release of their code,
a snapshot of their data for a legal or accounting report,
a copy to share with a family member, friend or colleague,
a regular backup for safekeeping, etc.

In the Orthogonal Persistence paradigm, publishing is managed by explicitly
_tagging_ a version of their data as suitable for sharing,
and granting select others a capability to access the tagged version
on a replica of their persistence domain.

This is in contrast with the Manual Persistence paradigm, where publishing
involves making a copy of a file or directory, or an archive of that directory
(zip file, tarball, etc.). The many versions of these files are easily confused
with each other, and their names and metadata often lack information, or
carry incorrect metainformation that indicates the time of last copy rather than
original publication. It is often hard to tell which copy came before or after
which other, which was extracted from a branch with non-standard changes,
which was already visited or is otherwise redundant, which is important to view,
what are the changes, whether the copy has its data intact and complete,
or what tampered with, corrupted or incomplete or garbled beyond the ability
to restore any useful data, etc.

Thus, in Orthogonal Persistence you never have to *save* a file,
yet you still once in a while have to *name* or *tag* a version,
as you would with e.g. `git tag` in a version control system.
Your Continuous Integration (CI) system may also add metadata
to indicate which versions passes various test suites,
and whatever review and action flows involving humans happen
would also track which version of which data corresponds to which
step of which process.
Once the persistence of the data has been made safe and
decoupled from the mechanism of publication, instead of being
the most expensive and unreliable step that trumps other concerns,
it becomes more obvious what the actual needs of publication are,
and how to address them in a the most adequate way.

### Resource Exhaustion

Whether in the paradigm of Orthogonal Persistence or Manual Persistence,
the underlying resources are ultimately the same, and finite.
And eventually, they tarry.

Now, inasmuch as Orthogonal Persistence is managed automatically,
without direct understanding of the human user's ultimate intent,
and with less care and attention given by humans
(which is the very purpose of it),
it will generally be somewhat less efficient in its use of resources than
a carefully curated manual persistent store would be
(say by a factor 2x or so for a relatively simple algorithm).
On the other hand, because Orthogonal Persistence makes more sense
than Manual Persistence, it can be less wasteful in other ways,
avoiding duplication data between multiple “uninstalled”, “installed”,
“published” or “archived” versions of some program or document,
or proliferation of now-redundant code for manual persistence, etc.
Either way, resource exhaustion a serious issue
with Orthogonal Persistence as with Manual Persistence.

With Manual Persistence, the issue can be handled by programs at risk of
filling up disk space, but seldom is. Instead, users or administrators
have to inspect disk space, clean up temporary areas,
remove unneeded system packages,
delete redundant copies and variants of programs and documents, etc.,
hopefully without deleting by mistake any essential information that wasn’t
properly backed up already.

With Orthogonal Persistence, the persistence usually happens
without user supervision, so instead users configure which processes run
in which division of what persistence domain, and assign
limits, alerts, shared or private soft or hard reserves, and
emergency plans to each domain:
every process is monitored so users or automated business agents
can take proactive measures before they run out of bounds, and
reactive measures after they do;
processes are stopped when they run out of space, and
can be resumed after space issues are solved;
space is reserved so special debugging processes can run
in the space of a stopped process, inspect and cleanup its data,
garbage collect unneeded elements, and restart the process—or
simply extract the useful data, restart a new process and
shutter the one that ran out of space.

### Schema Upgrade

The Schema is the set of all tables in a database,
of all concrete object types in a persistent heap,
of all file formats in a filesystem, etc.,
and the way they encode the data that matters to users.
Schema Upgrade is what happens when the code for the application changes,
and with it the way data is represented, yet the pre-existing data
and the execution threads that process it are not dropped on the ground
and forgotten or made to fail, their invariants broken,
their variants stuck never to decrease again, but instead upgraded,
made to follow the new invariants and variants,
continuing the operations as if nothing had changed,
or at least, not for the worse, only for the better.

Certainly there are cases when the Right Thing to do is indeed
to erase some obsolete data, and stop some stale processes.
But in general, most existing data, should be preserved, especially so
if it concerns inventory, accounting, legal matters,
incomplete orders, on-going operations, etc.—anything current
about the human-meaningful activity being represented and operated.

In the paradigm of Manual Persistence, any non-trivial Schema Upgrade is
a catastrophic event, to be specially handled by some crack team,
involving some ad hoc migration code, and often down time, voluntary or in-.
To avoid such stress, developers often choose to restrict themselves to
the “trivial” changes, only adding new tables and new fields initialized to NULL,
wherein the complexity of the Schema only ever increases and edge cases accumulate,
until the entire Schema is a mess that no one truly understands,
filled with obsolete or broken data
that poisons attempts at systematizing new processes.

In the paradigm of Orthogonal Persistence,
Schema Upgrade is a regular part of the development activity.
Variables and fields being added include initial or default values or formulas;
class redefinitions include migration methods;
regression tests include data migration tests; etc.
A whole lot of code can be automatically generated that
without Orthogonal Persistence is usually written by hand—just by
not forgetting the code history, and instead making schema evolution
a regular part of the programming language.

## Why Orthogonal Persistence Matters

### Automation vs Manual Labor

The opposition between Orthogonal Persistence and Manual Persistence
is somewhat analogous to automatic memory management
(with a garbage collector and/or declared ownership types)
vs manual memory management (with malloc and free).

In the prevalent paradigms of manual labor,
one crucial aspect of software is managed through
a lot of expensive and error-prone human labor.
Humans must repetitively reproduce low-level usage patterns everywhere locally,
yet with a lot of subtle and often hidden high-level global constraints
that can’t fit in any human’s brain except in the simplest cases.
Not only does this manual labor introduce exorbitant development costs,
the mismatch between the laborers’ capabilities and the task at hand mean
a countless bugs, and an endless stream of critical security vulnerabilities.
Down the line, users must waste enormous time in coping strategies,
or fail to, with sometimes catastrophic consequences.

In the paradigm of automation, the same aspect of software
is managed automatically, through a strategy coded once and for all,
that is correct by construction.
The algorithm is applied relentlessly by a computer that never gets bored
nor distracted, and will always enforce the global constraints
that it never misses of confuses even in the subtlest of edge cases.
Things Just Work for developers and users alike.
What few bugs may exist in the algorithm are not application-specific,
thus appear everywhere, can be detected early by anyone, fixed for everyone.

There are many cases when the provided automatic strategy
doesn’t perform as well as can a carefully implemented manual strategy.
However, those who want extra performance can always get it,
by improving the automation, adding performance annotations,
writing code in a style that enables the required optimizations,
or using an escape mechanism into manual management.
At the very worst they will be back to the previous paradigm—but
only for the small subdomain of cases for which they need utmost
performance, that they know and care about and can afford to optimize,
instead of everywhere, including the vast majority of aspects of the software
they don’t have resources or interest to get correct much less optimize.

### Modularity

Modularity is the ability to think about software in small chunks, locally,
each independently from the rest of the software,
except for a number of small delimited interfaces through which
it interacts with that rest of the software.

Orthogonal Persistence has atomic sections, write barriers and
persistent processes, that are modular.
Manual Persistence has transactions, commits and sagas, that aren’t.

Atomic sections are modular, because you can call code in a different module
without even having to know whether or not it contains atomic sections.
You have to be careful when writing an atomic section, but it’s care
you would need to put in anyway, and the developer who has to do it
also “owns” all the data at stake so has matching powers and responsibilities.
Similarly, write barriers are modular because you can freely call other
functions in other modules without having to care whether or not they contain
write barriers. You may have to care that the data that is being modified
cannot be observed in the meantime by other modules, but that is regular
data ownership, modularity, and mutual exclusion locks around data access.
These locks in turn are modular precisely because the processes that hold
and release the locks are persistent, and will thus respect the invariants
(coherent use) and variants (eventual release) of the locks. Persistent
processes are modular because their invariants and variants persist, and
so computer crashes do not cause deadlocks (data left forever unavailable),
corruption (invariants “temporarily” broken left unrestorable), or other
failures of software safety and liveness. Programmers don’t have to care
about a fragile ecosystem and how to properly restart transient processes
and recover lost context, because the context is not lost, the processes
are not transient, and the ecosystem is robust.

By contrast, transactions are not modular because every function needs
to know whether it’s already in a transaction or not, to be conscious
of what global entry point in a completely different module owns the
transaction. It’s like you’re always in an giant atomic section that involves
modules you don’t know about about data you don’t own yet must somehow respect.
Some systems allow for “nested” transactions, but it’s really a lie because
your code has to work against the least of the warranties of whether you’re
already in a transaction or not and your code will be interrupted or not,
and the simplest and most important modular programming mechanism,
having a function return a value, cannot be protected by those
nested transactions. There is no direct and safe way to schedule follow-up
code to be executed after some effects are persisted. Any client-side follow-up
after a commit is guaranteed to not be guaranteed to run. You can use
database queues (expensive to emulate if not builtin) to commit events
for follow-ups, and some daemon clients to process those events,
but then you need to reimplement the entire persistent execution engine
and encode the state of your continuations in those events—and/or
limit your follow-ups to what restricted subset of an execution engine
you could write. Reifying continuation state is also highly unmodular.
And having code run in transient clients is similarly unmodular.

## XXX

deciding when to start or end an atomic section only requires
local knowledge of the data structure at hand;
deciding when to start or end a atomic section requires
global knowledge of the entire program.
Composing atomic section is straightforward;
composing transactions is an undecidable problem.
Large programs with lots of atomic sections are simple;
large programs with lots of transactions (a.k.a. “sagas”) are extremely complex.

On the one hand, every database change needs to be within a transaction;
but on the other hand, it’s seldom clear where to start a transaction,

on the one hand it means the changes within will be persisted;
on the other hand, the end of a nested transaction doesn’t guarantee
that the data will be committed unless it’s the outermost transaction,
and there’s no way to escape from the transaction.
Long transactions, or lots of small ones...
higher-order functions...

you can’t compose transactions into larger transactions;
you can’t reason locally about transactions;
there is no good causal model within or across transactions;
transactions suppose that you already have a complete computation model outside transactions,
then builds a completely different one that interacts with it in weird ways, etc.

Meanwhile, _atomic sections_ integrate modularly as part of your regular computation model,

### Control Persistence

One reason things work composably with Orthogonal Persistence yet cannot with Manual Persistence is that we don't lose the execution context between transactions.

That code after the write barrier will be executed. The invariants won't be broken. The variants will be decreased.

Sure, with Manual Persistence, you can achieve the same: by using CPS, lambda-lifting then encoding of every continuation frame as well as heap object into the database to manually persist every thread.

Good luck with doing that manually. Have some wonderful time with SQL.

… and of course then you may have to reimplement your Garbage Collector and/or Ownership Model on top of SQL so as not to leak space as fast your persistent code can run.

Or then again, you could let compiler and its runtime VM automate all that for you (and more).


Moreover, changes to your control structure
will never be persisted, unless you explicitly transform your code
in CPS, lambda-lift it, make each frame a table in your schema,
and explicitly save each return frame in the database
at every function invocation, and delete it at every function return,
all operations practically impossible to handle manually without mistakes.


  b. Thou shalt never wait for a transaction to be committed from within a transaction.
     But you can declare an atomic block wherein transactions cannot be committed
     from within an atomic block wherein transactions cannot be committed.
     Make sure your atomic blocks are short, though.

  4. Processes may use Write Barriers (in the style of fdatasync underneath)
     to ensure that they won’t continue before the data so far was successfully
     persisted to transactional storage with enough confirmations.
     Thus, a process will fully persist all cryptographic secrets before it
     sends network messages that rely on those secrets being available.

  5. Processes within the same persistence domain can interact in synchronous
     ways that simultaneously modify multiple processes in a same transaction.
     Processes in distinct persistence domains can only interact via
     asynchronous messaging. Joint domains can be implemented on top of
     multiple asynchronous domains by using two-phase-commit (2pc)
     and other consensus protocols.

  6. An activity is persistent iff it is fully reconstituted from state
     committed in previous transactions upon system restart. For arbitrary
     computations, this means saving their continuation (function call frames
     including temporary variables), as well as explicit program “data”.
     Without compiler support you’d have to do CPS transformations by hand
     (Continuation-Passing Style), to manually reify continuations into data.
     A framework is provided to help you reify your program state manually
     if you choose that route, which has the advantage of working
     in any language, but is somewhat awkward to use.

  7. To persist, all programs are run in a suitable persistent virtual machine.
     It can be a high-level machine (e.g. source code, as in some databases),
     or low-level (e.g. WASM, as in Golem.cloud). In this package, we offer
     a high-level virtual machine based on the lambda-calculus and a graph
     data model.

  8. The actions that the processor takes based on the committed history
     must be logically idempotent as far as user-visible effects are concerned.
     Indeed, due to interruptions and restarts, a given action may be taken
     many times after its triggering condition was committed, yet before
     a sufficient reaction was committed. Thus, it should be possible to
     take these actions many times with no adverse effect.

  9. Typically, you’ll commit a message locally, including any serial numbers
     and random nonces, before you send it out, and you’ll retry sending
     until you have committed an acknowledgement.
     Conversely, you’ll commit a message before sending an acknowledgement,
     and you’ll re-send the acknowledgement if you wake up within the window
     of time that it should be sent.

### Corollaries

  a. An activity with bounded-size state is a finite state machine.
     More complex activities involve recursive data structures,
     including some that may represent the persisted control structures
     e.g. stack frames, as organized in continuations, threads, etc.
     Code evolves often and easily, which involves lots of according changes
     to the structure of implicit continuations as well as
     to that of explicit data.
     Rigid flat (non-recursive) database schemas are fundamentally insufficient
     to store the state of (recursive) activities,
     even less so if also manually curated.
     You can still have your nice curated databases in separate domains.

  c. You can also have write barriers
  The "activity thread that enacts the consequences of the transaction"
     shall be restarted identically whether the transaction just happened
     or it was committed in a previous run from which the present process was restarted.
     Its behavior must only depend on persisted data, and any other variable it uses
     must be reconstitutable equivalently from persisted data.

  d. The processor may optimistically make computations based on uncommitted data,
     but they will be that: optimistic.
     If the pipeline to committing is long, though, they may want to push data down that pipeline
     before its dependencies are committed --- but then, these messages
     will be marked as optimistic, their validity itself subject to the original message being committed.
     Importantly, the validity of commitments in an outbound message must be no more than the validity
     of the messages it depends on.

  e. An asynchronous processor may never block requests;
     conversely requests shall never be made that assume uncommitted data to have been committed
     when it wasn’t. If the assumption is required, just wait: it’s OK.
     If committing early is required, have the effects be conditional.

  f. When the process wakes up, it shall reactivate all potential interrupted activitities,
     re-send all messages marked for sending that haven’t timed out yet,
     and poll all the information sources for messages it may have missed.

## Consequences of violating those laws

Here is a deadlock situation I had to deal with:

  1. The `(User croesus)` activity is resumed in dbtx 1.
     This launches a manager thread, which asynchronously activates
     the activity `(TransactionTracker croesus 0)`,
     then synchronously waits for dbtx 1 to be committed.

  2. Meanwhile, the `(TransactionTracker croesus 0)` completes its transaction, then
     in dbtx 2 both stores the result, then tries to notify `(User croesus)`
     that it must deregister the completed tracker
     and handle the next requested transaction if any.
     Unhappily, the `(User croesus)` was stuck trying to wait for the dbtx 1 to be committed,
     which stops it from processing the dbtx 2 request,
     which prevents dbtx 2 from being committed,
     which blocks the current batch, so dbtx 1 will never be committed.

A solution was for the TransactionTracker activity to asynchronously message the User activity,
that would start a transaction when it’s ready.

## An unforgiving environment

Note that situations such as the above are made to break only more obviously
with my naive implementation of transaction batching on top of leveldb:

  * Multiple transactions are grouped into a same leveldb "write-batch".

  * To ensure transactionality, the batch is blocked if any transaction is still open.

  * To ensure liveness, new transactions are blocked while a batch is blocked.

  * A transaction waiting on another uncommitted transaction to commit
    will thus *always* cause a deadlock of the system.

A trivial and completely inefficient implementation would commit and wait after each transaction,
and block all new transactions while one is active.

A complex and somewhat inefficient implementation but one that simplifies coding somewhat
would dynamically discover and avoid conflicts, rollback, pause and restart transactions, etc.
In a system where transactions can be rolled back and tried again,
the second transaction could be made to fail (at least temporarily)
so that the first one gets a chance to be committed.
Whatever message that second transaction was trying to process would then have to be processed again,
with enough priority (based on arrival time) that it doesn’t get deferred indefinitely, breaking liveness.
But that’s not what we have with leveldb right now,
and we should probably look at a more advanced database
for this level of functionality.

More clever, we could use reversible representations for computations
that haven’t yet been confirmed yet.
In case of multiple concurrent write-capable replicas, use CRDTs, even.
When some data has been confirmed, old reversal information can be archived or erased.
Thus, instead of remembering THAT a one-time-switch was triggered, remember
which transaction or batch number it is to be committed at/with, and discard that information
only after it was actually committed.
Bigger changes can be a parallel composition of series of opposite such switches.
Other possibilities abound.
Thus, the set of active transactions could be a set of transactions, each with a state (active or not)
and a number (that tells when the change happens), plus a demon that advances things
based on what has been committed so far.
