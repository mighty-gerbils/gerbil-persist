# Persistent Concurrent Activities

## Some laws of persistent concurrent activities

  1. Activities persist data in *transactions*. Transactions must be short
     (e.g. take only a millisecond or so between start and finish).
     Long transactions are a big no-no.

  2. Each transaction has a clear single *owner*, an activity that starts it,
     is responsible for committing it, and for keeping it short.

  3. An activity that may ever block while waiting on I/O is *synchronous*.
     (This includes waiting for a database commit.)
     Of all the activities that persist their state in a given transaction,
     only the owner may be synchronous.

  4. Synchronous activities interact with each other through *asynchronous* messages
     stored in queues. Asynchronous message queues are asynchronous activities
     separate from the synchronous activities that may dequeue from them.

  5. An activity is persistent iff it is fully reconstituted from state committed
     in previous transactions upon system restart. For arbitrary computations, this means
     saving the state of their function call frames including temporary variables.
     Without compiler support you'll have to do a whole lot of CPS transformations by hand,
     that you'll then have to reify and reflect back from.

  6. The actions that the processor takes based on the committed history must be idempotent.
     Indeed, due to interruptions and restarts, a given action may be taken many times
     after its triggering condition was committed, yet before a sufficient reaction was committed.
     Thus, it should be possible to take these actions many times with no adverse effect.
     Typically, you'll commit a message locally, including any serial numbers and random nonces,
     before you send it out, and you'll retry sending until you have committed an acknowledgement.
     Conversely, you'll commit a message before sending an acknowledgement, and you'll re-send
     the acknowledgement if you wake up within the window of time that it should be sent.

### Corollaries

  a. An activity with bounded-size state is a finite state machine.
     More complex activities involve recursive data structures,
     including some that may represent the persisted control structures
     e.g. stack frames, as organized in continuations, threads, etc.

  b. Thou shalt never wait for a transaction to be committed from within a transaction.
     Instead, to keep transactions short and ensure single synchronous owner,
     thou shalt always wait for a transaction from within an activity thread
     that enacts the consequences of the transaction.

  c. The "activity thread that enacts the consequences of the transaction"
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
     when it wasn't. If the assumption is required, just wait: it's OK.
     If committing early is required, have the effects be conditional.

  f. When the process wakes up, it shall reactivate all potential interrupted activitities,
     re-send all messages marked for sending that haven't timed out yet,
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
that would start a transaction when it's ready.

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
with enough priority (based on arrival time) that it doesn't get deferred indefinitely, breaking liveness.
But that's not what we have with leveldb right now,
and we should probably look at a more advanced database
for this level of functionality.

More clever, we could use reversible representations for computations
that haven't yet been confirmed yet.
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

## QUESTIONS

* When we persist anything, there needs be a clear continuation that can be deduced from it.
  How do we ensure that in utility functions that are tempted to create a transaction?

* Maybe with-committed-tx should not be nestable, but with-tx should be?
