;; Handle concurrent user-level transactions atop a single serialized system transaction.
;; See the Concurrency Model section in the README.
(export #t)
(import
  :gerbil/gambit/threads
  :std/assert
  :std/misc/completion :std/misc/list :std/misc/number
  :std/sugar
  :clan/base :clan/concurrency :clan/path :clan/path-config
  :clan/persist/kvs :clan/persist/kvs-sqlite)

(defstruct KvsMux ;; multiplexing users for a key value store
  (kvs ;; underlying key value store
   name mx txcounter
   blocked-transactions open-transactions pending-transactions hooks
   batch-id batch batch-completion manager timer
   ready? triggered?)
  constructor: :init!)
(defmethod {:init! KvsMux}
  (lambda (self name (kvs #f))
    (unless kvs
      (set! kvs (make-KvsSqlite (ensure-absolute-path name persistent-directory))))
    (def mx (make-mutex name)) ;; Mutex
    (def txcounter 0) ;; Nat
    (def hooks (make-hash-table)) ;; (Table (<-) <- Any)
    (def blocked-transactions []) ;; (List Transaction)
    (def open-transactions (make-hash-table)) ;; mutable (HashSet Transaction)
    (def pending-transactions []) ;; (List Transaction)
    (def cache (make-hash-table)) ;; mutable (Table Bytes <- Bytes)
    (def batch-id 0) ;; Nat
    (def batch-completion (make-completion '(db-batch 0)))
    (def ready? #t) ;; Bool
    (def triggered? #f) ;; Bool
    (def manager true #;(db-manager self)) ;; Thread
    (def timer #f) ;; (Or Thread '#f)
    (struct-instance-init!
     self name kvs mx txcounter
     blocked-transactions open-transactions pending-transactions hooks
     cache batch-id batch-completion manager timer
     ready? triggered?)))

(def (open-db-connection name . opts) (apply make-KvsMux name opts))
(def (open-db-connection! name . opts) (current-db-connection (apply open-db-connection name opts)))
(defrules with-db-lock ()
  ((_ (km) body ...) (with-lock (KvsMux-mx km) (lambda () body ...)))
  ((_ () body ...) (with-db-lock (current-db-connection) body ...)))
(def (call-with-db-lock fun (db (current-db-connection)))
  (with-db-lock (db) (fun db)))

#|
(defmethod {close KvsMux}
  (lambda (self)
    (with-db-lock (self)
      (register-commit-hook!
       'close
       (lambda _
         {close (KvsMux-kvs self)}
         (thread-send (KvsMux-manager self) #f))
       self)
      (db-trigger! self))
    (thread-join! (KvsMux-manager self))))

;; Register post-commit finalizer actions to be run after this batch commits,
;; with the batch id as a parameter.
;; The hook is called synchronously, but it if you use asynchronous message passing,
;; it is possible that the hooks may be called out of order.
;; ASSUMES YOU'RE HOLDING THE KVSMUX-LOCK
;; Unit <- Any (<- Nat) KvsMux
(def (register-commit-hook! name hook (c (current-db-connection)))
  (hash-put! (KvsMux-hooks c) name hook))

;; Mark the current batch as triggered, because either some transaction must be committed,
;; or a timer has hit since content was added, or we're closing the database.
;; ASSUMES YOU'RE HOLDING THE LOCK
;; : <- KvsMux
(def (db-trigger! c)
  (if (and (KvsMux-ready? c) (zero? (hash-length (KvsMux-open-transactions c))))
    (finalize-batch! c)
    (set! (KvsMux-triggered? c) #t)))

;; Fork a system thread to handle the commit;
;; when it's done, wakeup the wait-on-batch-commit completion
(def (finalize-batch! c)
  (def batch-id (KvsMux-batch-id c))
  (def batch (KvsMux-batch c))
  (def batch-completion (KvsMux-batch-completion c))
  (def hooks (hash-values (KvsMux-hooks c)))
  (def blocked-transactions (KvsMux-blocked-transactions c))
  (def pending-transactions (KvsMux-pending-transactions c))
  (set! (KvsMux-batch-id c) (1+ batch-id))
  (set! (KvsMux-batch c) (leveldb-writebatch))
  (set! (KvsMux-batch-completion c) (make-completion `(db-batch , (KvsMux-batch-id c))))
  (set! (KvsMux-pending-transactions c) [])
  (set! (KvsMux-blocked-transactions c) [])
  (set! (KvsMux-ready? c) #f)
  (set! (KvsMux-triggered? c) #f)
  (set! (KvsMux-timer c) #f)
  (for-each (lambda (tx)
              (set! (KvsMuxTx-status tx) 'open)
              (hash-put! (KvsMux-open-transactions c) (KvsMuxTx-txid tx) tx))
            blocked-transactions)
  (thread-send (KvsMux-manager c) [batch-id batch batch-completion hooks pending-transactions]))

;;(def (call-with-db fun name)
;;  (def c (open-db-connection name))
;;  (try
;;   (parameterize ((current-db-connection c))
;;     (fun c))
;;   (finally (close-db-connection c))))
;;(defrule (with-db-connection (c name ...) body ...)
;;  (call-with-db-connection (lambda (c) body ...) name ...))
;;(def (ensure-db-connection name)
;;  (def c (current-db-connection))
;;  (if c
;;    (assert! (equal? (KvsMux-name c) name))
;;    (open-db-connection! name)))

;; 50-100 transactions per second is about what we expect on a typical disk.
;; : Real
(def deferred-db-trigger-interval-in-seconds .02)

;; ASSUMES YOU'RE HOLDING THE LOCK
;; : <- KvsMux
(def (deferred-db-trigger! c)
  (unless (KvsMux-timer c)
    (let (batch-id (KvsMux-batch-id c))
      (set! (KvsMux-timer c)
        (spawn/name/logged
         ['timer batch-id]
         (lambda () (thread-sleep! deferred-db-trigger-interval-in-seconds)
            (with-db-lock (c)
              (when (equal? batch-id (KvsMux-batch-id c))
                (db-trigger! c)))))))))

;; status: blocked open pending complete
;; When opening a transaction, it may be blocked at first so the previous batch may be completed,
;; but by the time it is returned to the user, it is in open status;
;; when it is closed, it becomes pending until its batch is committed,
;; at which point it becomes complete and any thread sync'ing on it will be awakened.
(defstruct KvsMuxTx (km txid status) transparent: #t)
(def (KvsMuxTx-completion tx)
  (def c (KvsMuxTx-km tx))
  (with-db-lock (c)
    (case (KvsMuxTx-status tx)
      ((open pending)
       (KvsMux-batch-completion c))
      (else #f))))

;; : <- (OrFalse Completion)
(def (wait-completion completion)
  (when completion (completion-wait! completion)))

;; Open Transaction
;; TODO: assert that the transaction_counter never wraps around?
;; Or check and block further transactions when it does, before resetting the counter? *)
;; TODO: commenting out the ready && triggered helps detect / enact deadlocks when running
;; tests, by having only one active transaction at a time; but then the hold can and
;; should be released as soon as "the" transaction is complete, unless we're already both
;; ready && triggered for the next batch commit. Have an option for that?
(defmethod {begin-transaction KvsMux}
  (lambda (self)
    (defvalues (transaction completion)
      (with-db-lock (self)
        (let* ((txid (post-increment! (KvsMux-txcounter self)))
               (blocked? (and (KvsMux-ready? self) (KvsMux-triggered? self)))
               (status (if blocked? 'blocked 'open))
               (transaction (KvsMuxTx self txid status)))
          (if blocked?
            (push! transaction (KvsMux-blocked-transactions self))
            (hash-put! (KvsMux-open-transactions self) txid transaction))
          (values transaction (and blocked? (KvsMux-batch-completion self))))))
    (wait-completion completion) ;; wait without holding the lock
    transaction))

;; For now, let's
;; * Disallow nested transaction / auto-transactions. We want a clear transaction owner, and
;;   the type / signature of functions will ensure that there is always one.
;; * Return the result of the inner expression, after the transaction is closed but not committed.
;;   If you need to synchronize on the transaction, be sure to return it or otherwise memorize it,
;;   or use after-commit from within the body.
(def (call-with-tx fun km wait: (wait #f))
  (awhen (t (current-db-transaction))
    (error "Cannot nest transactions" t))
  (def tx {begin-transaction (or km (current-db-connection))})
  (try
   (parameterize ((current-db-transaction tx))
     (fun tx))
   (finally
    (close-transaction tx)
    (when wait (sync-transaction tx)))))
(defrule (with-tx (tx dbc ...) body ...)
  (call-with-tx (lambda (tx) body ...) dbc ...))
(defrule (without-tx body ...)
  (parameterize ((current-db-transaction #f)) body ...))

(def (call-with-committed-tx fun (c #f))
  (call-with-tx fun c wait: #t))
(defrule (with-committed-tx (tx dbc ...) body ...)
  (call-with-committed-tx (lambda (tx) body ...) dbc ...))
(defrule (after-commit (tx) body ...)
  (without-tx (spawn/name/logged
               ['after-commit (KvsMuxTx-txid tx)]
               (lambda () (completion-wait! (KvsMuxTx-completion tx)) body ...))))

;; Mark a transaction as ready to be committed.
;; Return a completion that will be posted when the transaction is committed to disk.
;; The system must otherwise ensure that the action that follows this promise
;; will be restarted by a new instance of this program in case the process crashes after this commit,
;; or is otherwise some client's responsibility to restart if the program acts as a server.
(def (close-transaction (tx (current-db-transaction)))
  (match tx
    ((KvsMuxTx c txid status)
     (with-db-lock (c)
       (case status
         ((blocked open)
          (set! (KvsMuxTx-status tx) 'pending)
          (hash-remove! (KvsMux-open-transactions c) txid)
          (push! tx (KvsMux-pending-transactions c))
          (deferred-db-trigger! c)
          (KvsMux-batch-completion c))
         ((pending)
          (KvsMux-batch-completion c))
         (else #f))))
    (else (error "close-transaction: not a transaction" tx))))

;; Close a transaction, then wait for it to be committed.
(def (commit-transaction (transaction (current-db-transaction)))
  (wait-completion (close-transaction transaction)))

;; Sync to a transaction being committed.
;; Thou Shalt Not sync with the end of a transaction from within another transaction,
;; or you may deadlock, since that other transaction might be part of the same batch.
;; Instead, thou shalt sync on it in a background thread, that will then run
;; the very same code as you would if you would resume the persistent activity,
;; and that code must be effectively idempotent.
(def (sync-transaction (transaction (current-db-transaction)))
  (wait-completion (KvsMuxTx-completion transaction)))

(def leveldb-sync-write-options (leveldb-write-options sync: #f))

(def (db-manager c)
  (spawn/name/logged
   ['db-manager (KvsMux-name c)]
   (fun (db-manager-1)
     (let loop ()
       (match (thread-receive)
         ([batch-id batch batch-completion hooks pending-transactions]
          ;; TODO: run the leveldb-write in a different OS thread.
          (leveldb-write (KvsMux-kvs c) batch leveldb-sync-write-options)
          (for-each (lambda (tx) (set! (KvsMuxTx-status tx) 'complete))
                    pending-transactions)
          (for-each (lambda (hook) (hook batch-id)) hooks)
          (completion-post! batch-completion batch-id)
          (with-db-lock (c)
            (if (and (KvsMux-triggered? c) (zero? (hash-length (KvsMux-open-transactions c))))
              (finalize-batch! c)
              (set! (KvsMux-ready? c) #t)))
          (loop))
         (#f (void))
         (x (error "foo" x)))))))

;; Get the batch id: not just for testing,
;; but also, within a transaction, to get the id to prepare a hook,
;; e.g. to send newly committed but previously unsent messages.
(def (get-batch-id (c (current-db-connection)))
  (KvsMux-batch-id c))

(def (db-get key (tx (current-db-transaction)) (opts (leveldb-default-read-options)))
  (leveldb-get (KvsMux-kvs (KvsMuxTx-km tx)) key opts))
(def (db-key? key (tx (current-db-transaction)) (opts (leveldb-default-read-options)))
  (leveldb-key? (KvsMux-kvs (KvsMuxTx-km tx)) key opts))

(def (db-put! k v (tx (current-db-transaction)))
  (def c (KvsMuxTx-km tx))
  (with-db-lock (c)
    (leveldb-writebatch-put (KvsMux-batch c) k v)))
(def (db-put-many! l (tx (current-db-transaction)))
  (def c (KvsMuxTx-km tx))
  (with-db-lock (c)
    (let (batch (KvsMux-batch c))
      (for-each (match <> ([k . v] (leveldb-writebatch-put batch k v))) l))))
(def (db-delete! k (tx (current-db-transaction)))
  (def c (KvsMuxTx-km tx))
  (with-db-lock (c)
    (leveldb-writebatch-delete (KvsMux-batch c) k)))

#;(trace! current-db-connection current-db-transaction
        open-db-connection open-db-connection!
        close-db-connection! close-db-connection call-with-db-connection
        db-trigger! call-with-db-lock
        open-transaction call-with-tx call-with-committed-tx close-transaction
        commit-transaction register-commit-hook! db-manager finalize-batch!
        get-batch-id db-get db-key? db-put! db-put-many! db-delete!)
|#
