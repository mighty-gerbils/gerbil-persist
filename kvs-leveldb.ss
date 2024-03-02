;;;; Key Value Store Interface

(import
  (only-in :std/error check-argument)
  (only-in :std/misc/completion make-completion)
  (only-in :std/misc/number pre-increment!)
  (only-in :std/misc/path path-parent ensure-absolute-path)
  (only-in :clan/db/leveldb leveldb-open leveldb-default-options leveldb-get
           leveldb-writebatch leveldb-write leveldb-write-options leveldb-sync-write-options
           leveldb-writebatch-clear leveldb-writebatch-put leveldb-writebatch-delete)
  (only-in :clan/path-config persistent-directory)
  (only-in :clan/persist/kvs Kvs Kvs-connection))

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
    (check-argument (not (KvsLeveldb-batch self))
                    "KvsLevelDb without transaction already started" self)
    (def batch-id (pre-increment! (KvsLeveldb-batch-id self)))
    (set! (KvsLeveldb-batch self) (leveldb-writebatch))
    (set! (KvsLeveldb-batch-completion self) (make-completion `(db-batch ,batch-id)))))

(defmethod {abort-transaction KvsLeveldb}
  (lambda (self)
    (check-argument (KvsLeveldb-batch self)
                    "KvsLevelDb with transaction already started" self)
    (leveldb-writebatch-clear (KvsLeveldb-batch self))
    (set! (KvsLeveldb-batch self) #f)
    (set! (KvsLeveldb-batch-completion self) #f)))

(def leveldb-sync-write-options (leveldb-write-options sync: #f))

(defmethod {commit-transaction KvsLeveldb}
  (lambda (self)
    (check-argument (KvsLeveldb-batch self)
                    "KvsLevelDb with transaction already started" self)
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
