# Orthogonal Persistence, The Model

_Orthogonal Persistence_: You bind variable `x` to 42 at your REPL, but then
some “youths” steal your laptop while orcs raze your datacenter to the ground.
So you move to a safer place, get a new laptop, and enter your passphrase in it;
the system downloads encrypted backups from redundant datacenters on multiple
other continents; after a few minutes, your interface is restored to the
same state it was when you left it, and variable `x` is still bound to 42.
Yet not one single instructions in any of your applications ever had
to even mention anything about storage and retrieval.

[This paper presents an original re-framing of Orthogonal Persistence,
as refactored into a simple set of concepts in March 2024.]

## Orthogonal Persistence: Computations that Matter on Data that Matters

«Designing a programming language without data persistence means
“I only care about toy computations for people whose data doesn't matter.”
Designing a database without a good programming language means
“I only care about toy data not part of any computation that matters.”»

### Persistence by Default

Orthogonal Persistence (of Processes, or however named activities),
that John de Goes calls “Durable Execution”, is when
ongoing processes persist (at least by default),
such that if interrupted for any reason (memory exhaustion,
hardware fault, power interruption, machine destroyed or stolen, etc.),
they will automatically restart from last committed state.

Orthogonal Persistence is to be opposed to the prevalent paradigm of
Manual Persistence, wherein all data is transient by default, and
developers have to explicitly write data to one or several
disk files when they desire persistence,
and further organize their actions into database transactions
when they desire atomicity.
Orthogonal Persistence essentially reverses the burden of persistence:
all state changes are safely written to persistent storage by default,
and escaping into transience is the edge case sometimes invoked for performance.

Although most issues with Manual Persistence disappear with
Orthogonal Persistence, some issues remain, though in a simplified way,
reframed upside down into not having been Persistence issues after all,
so much as essential logical issues that had been previously drowned
among the cumbersome details of manual persistence:
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
_atomic sections_ (a.k.a. critical sections):
code that produces intermediate states that should not be
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

Note that atomicity is specific to a given persistence domain.
Changes across multiple persistence domains are not atomic by default.
In some cases, atomic changes across multiple persistence domains
can be achieved, but they require that the domains be under common management,
or at least to agree on some costly consensus protocol such as two-phase commit
(see section [Composing Persistence Domains](#composing-persistence-domains)).

### Synchronization

Synchronization is the ability to wait for some data to have been
persisted for sure before to take action that depends on it
(such as ensuring a transfer was complete before sending
acknowledgements, or signing a follow-up transaction, etc.).

In the Orthogonal Persistence paradigm, this is managed using explicit
_memory barriers_: an instruction or procedure that pauses evaluation
until after the changes so far are committed to (persistent) memory.
Optionally, it may be combined with thread-spawning to register
a callback function to be called after the current transaction commits,
all from within an atomic section.

Note that evaluation after a memory barrier can actually continue
optimistically after the memory barrier. However, the side-effects it produces
will not be observable by users unless and until after the changes are committed;
the output side-effects will be queued, and the input side-effects may block
execution until the memory barrier is passed and outputs dequeued.
The optimistic evaluation after a memory barrier may also not acquire locks
on resources still available before the memory barrier, only on new resources
created in the same optimistic “generation”.

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

Typically, before you send out a message that commits you
economically or legally or in any meaningful way,
you will want to make sure that any associated supporting document,
serial number, random nonce, transaction log, etc., is persisted.
And once you’re ready to send it out, you will retry sending it
until you have committed an acknowledgement that it was received.
Conversely, you’ll commit a message before sending an acknowledgement,
and you’ll re-send the acknowledgement if you wake up within the window
of time that it should be sent.

Note how multiple processes or threads within the same persistence domain
can interact in synchronous ways, with a shared underlying transaction
and memory barriers, always going forward, never having to rollback,
without any consistency problem. This is totally unlike the database
transaction model, where processes can never know that they are not going
to interfere with each other, with all but one having to be rolled back.
A lot of the needless complexity of database servers can be eschewed.
Usual mutexes and all the regular programming techniques of real
programming languages that people actually use and understand can be used
to deal with contention, including much simpler reimplementations of
any of the mechanisms that databases provide at the wrong place.

On the other hand, processes in distinct persistence domains must
communicate via asynchronous messages and abstractions built atop such.
Moreover, every asynchronous message sent to a persistent process must
(1) be idempotent, and (2) can only be sent after a memory barrier.
Indeed, due to transient failures and restarts, a given action may be taken
many times after its triggering condition was committed, yet before
a sufficient reaction was acknowledged. Thus, it should be possible
to take these actions many times with no adverse effect.
Meanwhile, messages sent to a transient process need be neither idempotent
nor wait for a memory barrier; however, the entire interaction with a transient
process must be reconstitutable from scratch at any point, since
the transient process may go at any time and have to be replaced
by a new one with empty state.

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
without direct understanding of the human user’s ultimate intent,
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
Either way, resource exhaustion is just as serious an issue
with Orthogonal Persistence as with Manual Persistence.

With Manual Persistence, the issue can be handled by programs at risk of
filling up disk space, but seldom is. Instead, users or administrators
have to inspect disk space, clean up temporary areas,
remove unneeded system packages,
delete redundant copies and variants of programs and documents, etc.,
hopefully without deleting by mistake any essential information that wasn’t
properly backed up already.

With Orthogonal Persistence, persistence usually happens
without user supervision, so instead users configure which processes run
in which division of what persistence domain, and assign
limits, alerts, shared or private soft or hard reserves, and
emergency plans to each domain:
every process and every domain is monitored, its resource usage accounted,
so users or automated business agents
can take proactive measures before they run out of bounds, and
reactive measures after they do;
processes are stopped when they run out of space, and
can be resumed after space issues are solved;
space is reserved so special debugging processes can run
inside the scope of a stopped process, inspect and cleanup its data,
garbage collect unneeded elements, and resume the process—or
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
to erase some obsolete data, and stop some stale processes;
but then that same cleanup could have been done at any point,
even without a schema upgrade, and independently from any other upgrade.
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

### Model Independence

Note how Orthogonal Persistence is not tied to any particular data model,
or, for that matter, from any particular model for a query language,
or for speculative potentially-conflicting concurrent transactions:
You can use flat tables in the style of SQL or Datalog if you like,
or a simple key-value store, a statically or dynamically typed object graph,
and even a raw untyped memory space, or a mix of the above,
or anything you like whatsoever, as fits whatever programming language you prefer to use.
Persistence is Orthogonal to the Data Model, and the Data Model is Orthogonal to Persistence.

It is quite an inefficient market that resulted in the ACID properties of Persistence
being sold in a package-deal with an absurd data model, the Relational Data Model,
that fits very few actual use cases;
What more, it uses a horribly misdesigned query language (SQL),
and there is no decent modification language, much less any attempt to standardize one;
instead, databases provide you with a dubious model for speculative transactions,
that allows them to improve their benchmarks, but only make things more complex for users.

Instead, users should be able to use whichever data model and concurrency models
fit their application, as specified in their regular programming language.
If “tables everywhere” were really the paradigm in which
it is most natural to think about a problem, programmers for this problem
would naturally want to use a table-oriented language, such as APL.
More likely, some of the data would be in tables, and other data would be
in different data structures, indexed differently, just as in most programs in most languages.
Whichever way, note that the appropriate language can *never* be SQL,
because SQL is not a complete programming language, not being able to modify the data.
The pathetic languages that database vendors typically come up with,
made by people who are definitely not programming language experts
or even actual amateurs, never make the cut, either.
(An amateur by definition loves what he’s doing;
those who misdesign these languages obviously have no love for this part of their job,
since they’re not even trying to do it right, or learn the very basics of language design.)
On the other hand, an appropriate language can conceivably have SQL embedded in it as a subset,
or hopefully something roughly equivalent to SQL but better designed,
such as C# with its LINQ extension.

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
countless bugs, and an endless stream of critical security vulnerabilities.
Down the line, users must waste enormous time in coping strategies—or
fail to, with too often catastrophic consequences.

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
(Some Functional Programmers call that Compositionality; same difference.)

Orthogonal Persistence has atomic sections, memory barriers and
persistent processes, that are modular:
they say “don't cut the computation here” which only depends on local knowledge and weak synchronization.
Manual Persistence has transactions, commits and sagas, that aren’t:
they say “cut the computation exactly here” which requires global knowledge and strong synchronization.

  - Atomic sections are modular, because you can call code or be called by code
    in a different module without even having to know whether or not
    it contains atomic sections.
  - You have to be careful when writing an atomic section, but it’s care
    you would need to put in anyway, and the developer who has to do it
    also “owns” all the data at stake and
    so has matching powers and responsibilities.
    Atomic sections only require local knowledge of the module
    in which they are written.
  - Similarly, memory barriers are modular because you can freely call
    functions in other modules without having to care whether or not
    they contain memory barriers.
  - When an activity of yours requires one or more memory barriers
    (possibly introduced by an indirect library module you don’t know about),
    it may be important to ensure that other activities will not concurrently
    observe the speculative data and assume it was persisted already.
    But this is easily fixed
    either by the other activity also using a memory barrier,
    or by using a mutual exclusion lock to ensure only one activity has access.
  - Mutual exclusion locks (mutexes) are modular once again because developers
    do not have to track down precisely which activity holds which locks
    (as long as a coherent lock discipline is enforced in general,
    which can be enforced with local knowledge).
    Indeed, mutual exclusion across memory barriers is possible precisely
    because the processes themselves are persistent, and so can be made
    to respect the invariants (coherent lock discipline) and variants
    (eventual release) of mutexes. By contrast, mutual exclusion across
    transactions is impossible, because clients do not persist but lose
    execution context, and therefore mutexes are *guaranteed* to
    eventually deadlock and leave the data they protect in a corrupted state.
  - Persistent processes are modular, because programmers can trust that
    every process will be robustly executed, its invariants preserved
    and its variants decreased, and thus need not be worried about any
    of these processes being stopped and lost in the middle of using some data
    other processes depend on, forever “temporarily” locked and corrupted,
    in a failure of safety and liveness.
  - Programmers don’t have to care about how to properly restart a entire zoo
    of transient processes in the correct dependency order to be confident that
    processes will resume correctly after a failure, correctly restoring all
    the required context that was previously correctly persisted, because
    the ecosystem is robust and already takes care of it for them.

Manual Persistence is unmodular for the very same reasons, in reverse:
  - You cannot reason locally about transactions;
    you cannot compose smaller transactions into larger transactions
    or decompose larger ones into smaller ones;
    there is no good causal model within or across transactions;
    the entire transaction model is flat and unmodular.
  - Transactions are not modular because every function needs to know whether
    it’s already in a transaction or not, to be conscious of what global entry
    point in a completely different module owns the transaction.
  - Transactions are like always being in an giant atomic section that involves
    modules you don’t know about, handling data you don’t own yet must somehow
    respect, while those modules must also magically respect data they know
    nothing about from other modules.
  - The model of a persistent database with transient clients supposes that
    you already have a complete transient computation incapable of persistence,
    then builds a completely different model for persistence without computations,
    and try to make the two interact weakly, yet expect a robust result.
  - Some systems allow for “nested” transactions, but it’s really a lie because
    your code has to work against the least of the warranties of whether you’re
    already in a transaction or not and your code will be interrupted or not.
  - Nested transactions do not support the simplest and most important modular
    programming mechanism, having a function return a value, which cannot be
    protected by those nested transactions.
  - Transactions and commits offer no direct and safe way to schedule follow-up
    code to be executed after some effects are persisted. Any client-side
    follow-up code after it is guaranteed to not be guaranteed to run,
    and multi-transaction “sagas” that run on such clients
    are intrinsically unreliable.
  - Attempts to avoid sequences of many transactions
    by doing a lot of work in a single long transaction
    generate more data contention as the transaction gets longer,
    with less reliability, and more latency for everyone.
    Attempts to divide these transactions in many “batches” require a lot
    of engineering efforts to regain some performance without addressing
    the fundamental problem. The entire speculative model wherein clients
    try and retry many times is unreliable, and yes, unmodular.
  - One safe but indirect way to schedule follow-up code execution is to use
    database queues (expensive to emulate if not builtin) to commit events
    that some background daemon will dequeue and act upon in a follow-up
    transactions. But then you need to encode the entire context of the
    follow-up in each event, which is tantamous to manually implementing
    an ad-hoc version of process persistence, just for that follow-up.
  - Manual implementation of process persistence is not only tedious,
    but itself not modular, unless all modules are required to manually
    partake in a complex persistence protocol, at which point you’re
    systematically implementing process persistence by hand with humans
    as bad, slow and unreliable compilers.
  - The manual persistence protocol itself is unmodular,
    as persistence will make many details of module code part of its interface,
    and require lots of other modules to be modified whenever that code changes.
  - With or without follow-up internal events, Manual Persistence demands that
    either one process will be updated that handles all internal and external
    events, or multiple processes each for some kinds of events, that will each
    be supervised and managed and restarted in order without forgetting any.
    Either way, this requires a lot of advanced system administration and is
    extremely unmodular.

All in all, Manual Persistence makes transactional applications a nightmare
to design and maintain, and require a lot of coordination between developers
of notionally independent modules, database administrators, network
administrators, and a host of expensive infrastructure professionals.

### Atomic Section as Mutual Exclusion

There is not just a clear analogy but a common higher pattern
between atomicity of persistent changes and
atomicity of changes in the context of concurrent activities.

The widely accepted programming language solution for atomicity in the context of concurrency is
*mutual exclusion* locks.
The high-level primitive for them is some variant of a
`with-lock` primitive to execute a simple thunk while holding a lock.
The low-level primitive is an explicit lock object with which you manually
do the above and carefully handle failure in the absence of exceptions.
Either way, all your code is written in your usual programming language;
most of the code is blissfully unaware of when locks are used;
the few “critical sections” need be careful to respect a hierarchy of locks,
but they are written in the same familiar language, and are pretty small.
This primitive is thus modular: you can keep writing functions that don't have to care
whether other functions ever use the primitive.
The mutual exclusion mechanism is local and lightweight, efficient and scalable;
it doesn’t force any global reorganization of your code,
any specific data model, any specific control flow model.

If concurrency had a “solution” for atomicity similar to transactions
“solve” atomicity for databases, it would look like this:
instead of local `with-lock` thunks, you must organize your code in
global “transactions” thunks containing everything that affects application state
without starting or ending inside a `with-lock` statement.
That’s an inversion of control;
to be done manually in languages without call/cc.
Moreover, control flow outside transactions is considered unreliable, such that
any control flow state required to ensure application invariants must be
explicitly reified within the transaction (see Persisting Execution Context below).
To add insult to injury, the parts done while holding locks as well as
all changes that matter must be written as magic snippets
in a completely different language
that you must manually metaprogram with strings.
Furthermore, if you keep your transactions too small,
you will incur a lot overhead and kill performance,
whereas long transactions will kill both concurrency and liveness.
Finally, the data model for your transactions is completely different from
the regular data model for your programming language.
That’s what transactions for concurrency would be, and what transactions for databases are.

If this design sounds completely crazy, that’s because it is.
Yet that is what university professors haugthily teach,
what billion-dollar data expert businesses sell,
and what every else pays billions of dollars to buy and use.
Analyzing the ins and outs of this madness would require its own separate essay.

### Persisting Execution Context

In the dominating paradigm of Manual Persistence, it is relatively
straightforward to restart a database client after it fails,
though even this simple task leads to daunting complications with many
bad surprises when reliability or scalability are desired.
However, when doing so, all the execution context of the client is crucially
lost at each random or not-so-random failure.
Orthogonal Persistence preserves this context, and that is the main difference.
How does Orthogonal Persistence work, and what would it take
to achieve the same process persistence manually?

Process persistence consists in persisting not just
inactive, “dead”, data structures, but also
the active “live” control structures of the processes that manipulate them:
the virtual machine “registers” and the control stack for every thread,
the shared data heap, the “file descriptors” or “handles”
opened with the Operating System, the libraries loaded,
any shared memory or resource held, any mutex at stake, etc.

The *low-level* way to persist data is to use some Virtual Machine (VM) that
will do it for you, such as [Golem](https://golem.cloud)’s WASM environment.
The VM implementation knows where all your data is, and can make regular
snapshots of the state of the virtual CPU and the associated memory,
saving CPU registers and whichever memory pages have changed between two snapshots.
You can only enjoy the hardware accelerations supported
by the VM implementation, but there are a finite number of them,
and worse comes to worst you could run the truly computation-intensive parts
in a transient “coprocessor” process.
In particular, making coherent snapshots when a process runs with multiple threads,
possibly on multiple processors, can be especially tricky and costly,
but it is ultimately possible.
Note that while the virtual machine provides low-level persistence,
you will want the higher-level language running on top of it to provide
the ability to do schema upgrade: the language should support full schema dump and restore
for the sake of restarting from a clean image,
but maybe also dynamic class redefinition for the sake of faster schema upgrades.
As for incremental updates between snapshots,
they can be happen by following low-level sets of changes to the VM’s memory,
or by following deterministic VM reactions to high-level recorded input
(which is an approach known as “Prevalence” when done manually).

The *high-level* way to persist data is to write your programs in some language
that has a compiler that supports Orthogonal Persistence.
The compiler will then make each piece of a program’s execution context into
an explicit object that can be serialized and stored in an underlying database:
Continuation-Passing Style will make stack frames into objects;
lambda-lifting will make lexical scope into objects;
various explicit monads will make the context of as many effects into objects;
all stack-allocated and heap-allocated data must be tracked with suitable types,
such that pointers to other data can be suitably saved and restored.
Then, every low-level checkpointing transaction will
add the new context objects to the database and delete the old ones from it.
To detect which objects are old and can be deleted, and avert memory leaks,
the compiler will implement the static or dynamic model of ownership or liveness
of context objects in the programming language semantics.
The high-level approach may or may not perform better than the low-level approach
in persisting program; yet support for all the primitives and transformations described
may be required anyway to deal with Schema Upgrade.

A lot of the problems, solutions and workarounds will be the same whether
the persistence implementation is higher-level (more work done by the compiler)
or lower-level (more work done by the runtime environment).
Ultimately, you can see a high-level solution as having the compiler generate
a VM specialized for your program, instead of using a general-purpose
low-level VM for all programs.
There are costs and benefits to either approach with many tradeoffs, and
the “best” solution for you might lie in the middle.

Whether you choose a higher- or lower- level approach,
good luck with doing it manually. Have some wonderful time with SQL.
You’ll soon find that SQL’s rigid and flat (non-recursive) database schemas
are fundamentally insufficient to store the state of (recursive) activities,
even less so if also manually curated and you must either have thousands
of typed tables (one per frame), or eschew the normalization and typechecking
by the database.
Except for the simplest of programs, you’ll find that it is undoable
to implement manual persistence of processes without mistakes,
what more with the limited resources available to you—except by automating it,
at which point you have Orthogonal Persistence.
What more, even small changes in your program can lead to large changes
in the persistence model, that need to be correctly propagated everywhere;
manual persistence is not modular at all.

Finally, mind that running with Orthogonal Persistence requires no changes
to local-only applications, but does require small changes to network
applications that transact with remote servers.
These will be local, modular, modifications, but modifications still.
In particular, you will have to use the atomic section and memory barrier
APIs to ensure your program correctly persists its effects
when communicating with remote services.
Also, when a persistent process wakes up after a pause, interrupt, failure,
etc., it will reactivate all potential interrupted activitities, re-send
all messages marked for sending that haven’t timed out yet,
and poll all the information sources for messages it may have missed.
Your network protocols will hopefully support a mode when the re-sent
messages will be idempotent, and where the data sources can indeed be polled
for messages received since the last one committed.

### Composing Persistence Domains

Manually writing code to coherently persist data across several devices,
filesystems, databases, storage services, is extremely hard, and
will be fragile with respect to any change in the configuration.
It will require cooperation from every module that manually persists any data,
as well as special code to handle the outermost places that start and commit
transactions. This is undoable in practice.
At best, Manual Persistence will use some database service that is
configured to have replicas, as part of a single protocol,
with expensive system administrators to keep it all running.

With Orthogonal Persistence, the underlying store is already abstracted away
from every program for every user. The matter is then to be able to
(1) define configurations that variously compose persistence domains
that follow the protocol to create joins, shards, synchronous and
asynchronous replicas, n-out-m consensus, encrypted stores, etc., and
(2) seamlessly migrate data from one configuration to the other,
without interrupting any of the programs running in that domain.

This can be done in a generic fashion using domain combinators
that work for all programs for all users, without stopping any activity,
though maybe with increased latency and reduced speed during migration.
With the proper combinators, you can even have your nice curated database still
in one persistence domain, with more dynamic and less curated yet still persistent experiments
in a separate persistence domain (though possibly on the same server, so you can still
share transactions and not need an extra 2-Phase-Commit protocol).

## Long Range Issues with Persistence

Orthogonal Persistence completely solves “short range” persistence issues:
programmers do not have to explicitly open and close files and network connections,
read and write, marshal and unmarshal, encode and decode data,
organize their accesses in transactions,
metaprogram a “database language” using strings, etc.,
not to mention handle all the associated errors,
for their data to persist.
Most of the code having to deal with persistence,
which is a sizeable fraction of the code, disappears.
Is automated away. Humans do not have to spend brain cycles about it anymore.

But that means that mid- to long- range issues take the front stage:

  - Low-level languages, or “untrusted” low-level escapes from high-level languages,
    can *persistently* corrupt data, in ways that may only be detected long after the fact,
    after dancing a “fandango on core”.
    The need to survive low-level errors makes regular backups necessary,
    yet sometimes not sufficient. Scary.

  - Static languages will make Schema Upgrade hell by introducing much rigidity in the Schema,
    forcing expensive global transformations,
    introducing namespace issues to identify entities across schema upgrades,
    making incremental change harder and more expensive, etc.

  - Dynamic languages with suitable reflection primitives
    can solve the Schema Upgrade hell,
    but will lose in performance compared to low-level languages,
    and in safety compared to static languages.
    At least you can gain metaprogramming in exchange as a super-power,
    if you pick a Lisp instead of a blub language; yet most programmers fail to.

  - Partial code edits may corrupt the entire database by introducing
    broken inconsistent intermediate steps.
    Complete code edits may require omniscience of all code, libraries, etc.,
    and more work than possible by one person in one session.
    Dealing with code edits therefore necessitates
    database versioning to be able to branch with code and revert,
    and virtualization so that edits do not jeopardize the entire universe,
    but only update a sub-universe with localized changes.
    Transactionality in code updates may enable processes to see a consistent version of the code.

  - Reflection is necessary to inspect processes, modify them, salvage the runaway ones.
    Yet some form of protection from unauthorized tampering is necessary.
    This calls for some kind of capability-based architecture, to contain the powers
    that would otherwise allow bad code to corrupt the entire system.

  - Forms of PCLSRing with respect to *user* invariants (not just “system” invariants)
    are necessary to cleanly kill processes, but also to stop, inspect, migrate, upgrade them.
    This requires both system support, compiler support for every language, and language support
    to enable users to define those invariants in the interaction “language” (formal or informal)
    of *their* users.

  - Schema Upgrade requires high-level language support, or else users may find the hard way that
    they can't keep their data without having to badly reinvent
    all of the mechanisms of manual persistence just for the sake of upgrading their Schema.

  - Inasmuch as persistent software must run on top of transient operating systems,
    all the transient details like hostname, pid, file descriptors, etc.,
    must be virtualized away from the underlying transient operating system,
    since they might change from time to time as the process migrates
    to survive adversarial events.
    A "Persistent Operating System" might offer a Virtual Machine where they are replaced by
    some persistent handle that abstracts away the mapping between their persistent identity
    and an underlying transient implementation.
    A lower-level "Persistent Layer" might instead let the programmers (and their libraries)
    implement those abstractions themselves—but even then the transient hostname, pid and fds
    shall only be accessed from within a transient block that covers their lifetime,
    possibly as part of a dedicated transient thread (for e.g. file descriptors).

All these concerns exist with Manual Persistence as well as with Orthogonal Persistence.
But with Manual Persistence, the low-level concerns of getting any persistence at all
create so much work and slows down development so much that these higher-level concerns
appear secondary in comparison.
By saving developers from those lowly concerns,
Orthogonal Persistence elevates the struggle to write software,
rather than eliminates it.

## Bibliography

My [LambdaConf 2016 talk](https://www.youtube.com/watch?v=KsswTN2cCSc&t=250s)
discusses Orthogonal Persistence, based on chapters 2 to 5
of my blog [“Houyhnhnm Computing”](https://ngnghm.github.io)
(pronounced “Hunam Computing”).

[A Persistent System In Real Use: Experiences Of The First 13 Years](https://os.itec.kit.edu/65_2525.php), by Jochen Liedtke, IWOOS 1993. Even the processes are Persistent. See the
[Website](https://6xq.net/eumel/),
[Docs](https://github.com/PromyLOPh/eumel),
[Code](https://github.com/PromyLOPh/eumel-src) and
[Tools](https://github.com/PromyLOPh/eumel-tools)
for the persistent system
[EUMEL](https://archiveos.org/eumel/).
TODO: Find code for the persistence work done on top of
L3 (maybe included in or linked from Eumel repositories?)
and L4 (see Charm?).

Operating Systems that persist processes, such as
[KeyKOS](https://archiveos.org/keykos/),
[EROS](https://archiveos.org/eros/),
[Coyotos](https://archiveos.org/coyotos/),
[CapROS](https://archiveos.org/capros/),
[Grasshopper](https://archiveos.org/grasshopper/),
[Mungi](http://tunes.org/wiki/mungi.html),
[Charm](https://archiveos.org/charm/),
[BRiX](https://archiveos.org/brix/),
[Argon](https://archiveos.org/argon/)...

The Scottish School of Orthogonal Persistence, including systems such as
[PS-Algol](https://en.wikipedia.org/wiki/PS-algol),
[Napier88](https://en.wikipedia.org/wiki/Napier88),
[Ten15](https://en.wikipedia.org/wiki/Ten15), and other
historical British (mainly Scottish) persistent languages and systems.
However, note that the otherwise interesting 2009 retrospective article
“Orthogonal Persistence Revisited” by Alan Dearle, Graham N.C. Kirby and Ron Morrison
does *not* mention persistence of continuations, stacks, execution contexts, processes, etc.
Possibly because none of the systems mentioned had it.
It also claims that PS-Algol was the first Persistent language,
and fails to cite or credit Eumel whose language ELAN that may have been even earlier,
and that did persist processes.

[Acton](https://www.acton-lang.org/),
a “fault tolerant distributed programming platform for building mission critical systems”
based on a high-level language (active in 2024).

Recent systems (as of 2024) offering “Durable Execution” or “Durable Computing”,
to insist they cover processes, not just data:
[Golem](https://Golem.cloud),
a WASM-based execution platform to run highly reliable services
with Orthogonal Persistence, which they call “Durable Execution”;
in the same vein, [Temporal.io](https://temporal.io/),
[restate](https://restate.dev), [DBOS](https://DBOS.dev)…
they all rely on logging transaction, once in a while dumping a snapshot,
and restoring state by replaying transactions from the snapshot.

The Workshop on Persistent Object Systems,
The Workshop on Database Programming Languages,
old VLDB conferences around 2000,
IWOOS, ICOODB, SOSP…
gotta mine these conferences, and more.
(Be sure to include some reading about “Pointer Swizzling”.)

Recent work:
[TreeSLS: A Whole-system Persistent Microkernel with Tree-structured State Checkpoint on NVM](https://dl.acm.org/doi/10.1145/3600006.3613160), SOSP 2023
[Reducing Write Barrier Overheads for Orthogonal Persistence](https://dl.acm.org/doi/10.1145/3687997.3695646), SLE 2024

Question: see how [Unison](https://www.unison-lang.org/), [Dark](https://darklang.com/) or
[Val town](https://www.val.town/) and other “infrastructure included” languages
do or don’t handle persistence.

## Coda: Friendly vs Unfriendly Persistence

In today’s world (2024), all your data persists… on your enemies’ servers.
The big corporations and bureaucracies that try to manipulate you
know everything about you, and run AIs to analyze your behavior
to manipulate you even more into buying their stuff and obeying their orders.
Modern “apps”, that don’t have or need a “save” button anymore,
may superficially look to end-users as if they had Orthogonal Persistence,
but underneath everything uses Manual Persistence;
corporations can afford thousands of developers, database experts and system administrators
to make work it at scale, so as to spy on hundreds of millions of human cattle.
Even when they are not officially allowed to use the information,
they’ll use it to identify then target you, at which point they can use
“parallel reconstruction”, entrapment, harassment or just plain illegal means
to further oppress you.

The only person who forgets everything is… you.
You don’t have a good record trail of all your actions and transactions.
What record you have, you cannot search.
Every word said near the always-listening microphone
of the position-tracking device known as “your phone” is recorded,
and will be used against you, but you won’t be able to search
for those words and be reminded of conversations that matter to you.
If you liked a page, an article, a comment, a book, a movie, a game…
the contents may disappear at any time, if they haven’t disappeared already.
You may have paid for it, but the company on the other side may go out of business,
may cancel the service you were using, or deactivate that particular item you liked.
Worse, the contents may have been rewritten and sanitized
to fit the ideology of the day, in an Orwellian move.
With a bit of luck and a lot of effort, you might find some version of it on
[archive.org](https://archive.org)… if it still exists the day you need it,
its robots weren’t told not to archive that data, and
the data wasn’t removed by legal actions or threats.
Most of the games, demos and other programs you like or used to like,
even if you still have access to a copy, will not run anymore, because
they depend on a virtual machine that was obsoleted (e.g. Flash),
or they relied on some remote service that either has disappeared already
or will one day for sure eventually disappear.

As a user, or even as an independent developer, you cannot afford
to hire or become a database expert or a system administrator, much less both;
very few individuals can afford to run by themselves a complete stack
of all the software they might like to use,
and none can afford to modify all the software they use that others wrote
to suitably persist the data that matters into their database.
Your only hope, our only hope as private citizens, is that there shall be
a platform that automatically handles the persistence of every piece of data
you see, for every program you use, in a way that *you* and other individuals
can search and mine for patterns, while your communications with the rest of
the world go through obfuscation channels so that *they* cannot.

All your friendly processes, like Dory the fish,
forget everything after a short while.
All the enemy processes, like an elephant, remember everything.
Help me change that for you and for everyone:
Sponsor [@fare on GitHub](https://github.com/sponsors/fare).
