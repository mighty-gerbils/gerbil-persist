;;;; Key Value Store Interface

(import
  :std/assert :std/db/dbi :std/db/leveldb :std/error :std/sugar
  :std/misc/completion :std/misc/list :std/misc/number
  :clan/path :clan/path-config
  :clan/persist/kvs)

(export #t)

(defstruct (KvsLeveldb Kvs)
  (batch-id batch batch-completion)
  constructor: :init!)

(defmethod {:init! KvsLeveldb}
  (lambda (self path (opts (leveldb-default-options)))
    (def abspath (ensure-absolute-path path persistent-directory))
    (create-directory* (path-parent abspath))
    (struct-instance-init! self (leveldb-open abspath opts) 0 #f #f)))

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

(def leveldb-sync-write-options (leveldb-write-options sync: #f))

(defmethod {commit-transaction KvsLeveldb}
  (lambda (self)
    (assert! (KvsLeveldb-batch self))
    (leveldb-write (Kvs-connection self) (KvsLeveldb-batch self) leveldb-sync-write-options)
    (set! (KvsLeveldb-batch self) #f)
    (set! (KvsLeveldb-batch-completion self) #f)))

(defmethod {read-key KvsLeveldb}
  (lambda (K k)
    (def v (leveldb-get (Kvs-connection K) k))
    (values v (and v #t))))

(defmethod {write-key KvsLeveldb}
  (lambda (K k v)
    (def b (KvsLeveldb-batch K))
    (leveldb-writebatch-put b k v)))

(defmethod {delete-key KvsLeveldb}
  (lambda (K k)
    (leveldb-writebatch-delete (KvsLeveldb-batch K) k)))
