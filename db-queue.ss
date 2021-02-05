;; Persistent message queues on top of leveldb

;; TODO: accept Gerbil actor style rpc.shutdown messages

(export
  DbQueue? DbQueue-send! DbQueue-restore
  DbCommittedQueue? DbCommittedQueue-send! DbCommittedQueue-restore)
(import
  :gerbil/gambit/threads
  :std/db/leveldb
  :std/misc/completion :std/misc/deque :std/misc/list :std/misc/number :std/sugar
  :clan/base :clan/concurrency
  :clan/poo/object :clan/poo/mop :clan/poo/io :clan/poo/number :clan/poo/type
  ./db)

(def PairNatNat (Pair Nat Nat)) ;; used to represent start and length of queue

;; : Bytes <- Bytes Nat
(def (db-indexed-key db-key index)
  (u8vector-append db-key (bytes<- Nat index)))

;; A DB Queue
(defstruct DbQueue
  (mx ;; : Mutex
   key ;; : Bytes ;; db-key
   start ;; : Nat ;; Next index to dequeue
   length ;; : Nat ;; Number of items in the queue. end = start + length
   manager)) ;; : Thread ;; or should we have a condition variable instead?

;; Assumes we already have a lock of the queue object, and the tx is open
;; : <- DbQueue TX
(def (%DbQueue-update q tx)
  (db-put! (DbQueue-key q) (bytes<- PairNatNat (cons (DbQueue-start q) (DbQueue-length q))) tx))

;; Internal: wake up the manager of a queue that isn't empty anymore.
;; : <- DbQueue Any
(def (%DbQueue-wakeup q tag)
  (thread-send (DbQueue-manager q) tag))

;; Internal: wake up the manager of a queue that isn't empty anymore.
;; : <- DbQueue Any
(def (%DbQueue-send! q msg tx)
  (db-put! (db-indexed-key (DbQueue-key q) (+ (DbQueue-start q) (post-increment! (DbQueue-length q))))
           msg tx)
  (%DbQueue-update q tx))

;; Push a message into a DbQueue
;; Assumes that tx is open
;; : <- DbQueue Bytes TX
(def (DbQueue-send! q msg tx)
  (with-lock (DbQueue-mx q)
    (lambda ()
      (when (%DbQueue-empty? q) (%DbQueue-wakeup q #t))
      (%DbQueue-send! q msg tx))))

;; Internal: wake up the manager of a queue that isn't empty anymore.
;; : <- DbQueue Any
(def (%DbQueue-receive! q tx)
  (let* ((index (post-increment! (DbQueue-start q)))
         (db-key (db-indexed-key (DbQueue-key q) index)))
    (decrement! (DbQueue-length q))
    (begin0
        (db-get db-key tx)
      (db-delete! db-key tx) ;; or should we keep it indefinitely?
      (%DbQueue-update q tx))))

;; Atomically pop a message from a DbQueue, or return #f if empty.
;; Assumes that tx is open
;; : (OrFalse Bytes) <- DbQueue TX
(def (DbQueue-receive! q tx)
  (with-lock (DbQueue-mx q)
    (lambda ()
      (and (not (%DbQueue-empty? q)) (%DbQueue-receive! q tx)))))

;; Query whether a DbQueue is empty
;; Assumes we hold the lock on the queue
;; : Bool <- DbQueue
(def (%DbQueue-empty? q)
  (zero? (DbQueue-length q)))

;; Get the state of a DbQueue from the database, given its db-key
;; : (Pair Nat Nat) <- Bytes TX
(def (DbQueue-state db-key tx)
  (cond ((db-get db-key tx) => (cut <-bytes PairNatNat <>))
        (else '(0 . 0)))) ;; owl of you

;; Restore a DbQueue from its persisted state, or start a new one if none is present.
;; : DbQueue <- Any Bytes (<- Bytes TX)
(def (DbQueue-restore name db-key processor)
  (def q (match (with-tx (tx) (DbQueue-state db-key tx))
           ([start . length] (make-DbQueue (make-mutex name) db-key start length #f))))
  (def manager
    (spawn/name/logged
     name
     (lambda ()
       (while #t
         (thread-receive) ;; wait to be woken up
         (let/cc break
           (while #t
             (with-tx (tx)
               (def msg (DbQueue-receive! q tx))
               (unless msg (break))
               (processor msg tx))))))))
  (set! (DbQueue-manager q) manager)
  q)

;; DB Committed Queue: only dequeue things that were fully committed
(defstruct (DbCommittedQueue DbQueue)
  (committed-end ;; : Nat ;; Next index to not dequeue yet
   pending)) ;; : (Dequeue (Tuple Nat Completion Nat)) ;; dequeue of batch-id, batch-completion, end

(def (%DbCommittedQueue-update-pending q tx) ;; the end was increased, so add to pending
  (def c (DbTransaction-connection tx))
  (def batch-id (DbConnection-batch-id c))
  (def qp (DbCommittedQueue-pending q))
  (if (and (not (deque-empty? qp)) (= batch-id (car (peek-front qp))))
    (pop-front! qp) ;; only wakeup once per batch id
    (let (completion (DbConnection-batch-completion c))
      (spawn (lambda () (completion-wait! completion) (%DbQueue-wakeup q batch-id)))))
  (push-front! qp (cons batch-id (+ (DbQueue-start q) (DbQueue-length q)))))

;; Push a message into a DbCommittedQueue
;; Assumes we hold the lock on the q and that tx is open
;; : <- DbQueue Bytes TX
(def (DbCommittedQueue-send! q msg tx)
  (%DbQueue-send! q msg tx)
  (%DbQueue-update q tx)
  (%DbCommittedQueue-update-pending q tx))

;; Pop a message from a DbCommittedQueue
;; Assumes we hold the lock on the q, that the q is not empty, and that tx is open
;; : Bytes <- DbCommittedQueue TX
(def (DbCommittedQueue-receive! q tx)
  (with-lock (DbQueue-mx q)
    (lambda ()
      (and (not (%DbCommittedQueue-empty? q)) (%DbQueue-receive! q tx)))))

;; Query whether a DbCommittedQueue is empty
;; Assumes we hold the lock on the queue
;; : Bool <- DbCommittedQueue
(def (%DbCommittedQueue-empty? q)
  (>= (DbQueue-start q) (DbCommittedQueue-committed-end q)))

;; Restore a DbQueue from its persisted state, or start a new one if none is present.
;; : DbCommittedQueue <- Any Bytes (<- Nat Bytes TX)
(def (DbCommittedQueue-restore name db-key processor)
  (def q (match (with-tx (tx) (DbQueue-state db-key tx))
           ([start . length]
            (make-DbCommittedQueue
             (make-mutex name) db-key start length #f (+ start length) (make-deque)))))
  (def manager
    (spawn/name/logged
     name
     (lambda ()
       (while #t
         (let (batch-id (thread-receive))
           (with-lock (DbQueue-mx q)
             (fun (DbCommittedQueue-manager-confirm-batch-id)
               (def qp (DbCommittedQueue-pending q))
               (while (<= (car (peek-back qp)) batch-id)
                 (set! (DbCommittedQueue-committed-end q) (cdr (pop-back! qp)))))))
         (let/cc break
           (while #t
             (with-tx (tx)
               (def msg (DbCommittedQueue-receive! q tx))
               (unless msg (break))
               (processor msg tx))))))))
  (set! (DbQueue-manager q) manager)
  q)
