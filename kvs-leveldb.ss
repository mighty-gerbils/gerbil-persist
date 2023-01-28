;;;; Key Value Store Interface

(import
  :std/assert :std/db/dbi :std/db/leveldb :std/error :std/sugar
  :std/misc/completion :std/misc/list :std/misc/number
  :clan/path :clan/path-config
  :clan/persist/kvs)

(defstruct (KvsLeveldb Kvs)
  (batch-id batch batch-completion)
  constructor: :init!)

(defmethod {:init! KvsLeveldb}
  (lambda (self connection)
    (struct-instance-init!
     self connection
     0 #f #f)))

(def (kvs-leveldb-open path (opts (leveldb-default-options)))
  (def abspath (ensure-absolute-path path persistent-directory))
  (create-directory* (path-parent abspath))
  (make-KvsLeveldb (leveldb-open abspath opts)))

(defmethod {begin-transaction KvsLeveldb}
  (lambda (self)
    (assert! (not (KvsLeveldb-batch self)))
    (def batch-id (pre-increment! (KvsLeveldb-batch-id self)))
    (set! (KvsLeveldb-batch self) (leveldb-writebatch))
    (set! (KvsLeveldb-batch-completion self) (make-completion `(db-batch ,batch-id)))))

(defmethod {abort-transaction KvsLeveldb}
  (lambda (self)
    (assert! (not (KvsLeveldb-batch self)))
    (leveldb-writebatch-clear (KvsLeveldb-batch self))
    (set! (KvsLeveldb-batch self) #f)
    (set! (KvsLeveldb-batch-completion self) #f)))

(defmethod {commit-transaction KvsLeveldb}
  (lambda (self)
    (assert! (not (KvsLeveldb-batch self)))
    (leveldb-writebatch-clear (KvsLeveldb-batch self))
    (set! (KvsLeveldb-batch self) #f)
    (set! (KvsLeveldb-batch-completion self) #f)))

(defmethod {read-key KvsLeveldb}
  (lambda (K k)
    (try (values (leveldb-get (Kvs-connection K) k) #t)
         (catch leveldb-error? => (lambda (_) (values #f #f))))))

(defmethod {write-key KvsLeveldb}
  (lambda (K k v)
    (def b (KvsLeveldb-batch K))
    (leveldb-writebatch-put b k v)))

(defmethod {delete-key KvsLeveldb}
  (lambda (K k)
    (leveldb-writebatch-delete (KvsLeveldb-batch K) k)))
