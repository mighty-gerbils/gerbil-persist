;;;; Key Value Store Interface

(import
  :std/db/dbi :std/db/sqlite :std/db/_sqlite :std/iter :std/sugar
  :clan/path :clan/path-config
  :clan/persist/kvs)

(defstruct (KvsSql Kvs)
  (begin-tx-stmt commit-tx-stmt abort-tx-stmt
   read-stmt write-stmt delete-stmt))

(defmethod {:init! KvsSql}
  (lambda (self connection begin-tx-stmt commit-tx-stmt abort-tx-stmt read-stmt write-stmt delete-stmt)
    (struct-instance-init! self connection begin-tx-stmt commit-tx-stmt abort-tx-stmt read-stmt write-stmt delete-stmt)))

(defmethod {begin-transaction KvsSql} (lambda (self) (sql-exec (KvsSql-begin-tx-stmt self))))
(defmethod {abort-transaction KvsSql} (lambda (self) (sql-exec (KvsSql-abort-tx-stmt self))))
(defmethod {commit-transaction KvsSql} (lambda (self) (sql-exec (KvsSql-commit-tx-stmt self))))

(defmethod {read-key KvsSql}
  (lambda (K k)
    (def s (KvsSql-read-stmt K))
    {bind s k}
    (try
     (match {query-fetch s}
       ((eq? #!void) (values {query-row s} #t))
       ((eq? iter-end) (values #f #f)))
     (finally (sql-reset/clear s)))))

(defmethod {write-key KvsSql}
  (lambda (K k v)
    (def s (KvsSql-write-stmt K))
    {bind s k v}
    (try
     {exec s}
     (finally (sql-reset/clear s)))))

(defmethod {delete-key KvsSql}
  (lambda (K k)
    (def s (KvsSql-delete-stmt K))
    {bind s k}
    (try
     {exec s}
     (finally (sql-reset/clear s)))))

(defstruct (KvsSqlite KvsSql)
  (begin-tx-stmt commit-tx-stmt abort-tx-stmt
   read-stmt write-stmt delete-stmt))

(defmethod {open KvsSqlite}
  (lambda (self path (flags (fxior SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE)))
    (def abspath (ensure-absolute-path path persistent-directory))
    (create-directory* (path-parent abspath))
    (def connection (sqlite-open abspath flags))
    (sql-eval connection (string-append
                          "PRAGMA locking_mode = EXCLUSIVE ;"
                          "CREATE TABLE IF NOT EXISTS kvs ( "
                          "key BLOB PRIMARY KEY, "
                          "value BLOB NOT NULL ) "
                          "WITHOUT ROWID ;"))
    (struct-instance-init!
     self connection
     (sql-prepare connection "BEGIN IMMEDIATE TRANSACTION")
     (sql-prepare connection "COMMIT TRANSACTION")
     (sql-prepare connection "ROLLBACK TRANSACTION")
     (sql-prepare connection "SELECT value FROM kvs WHERE key = ?")
     (sql-prepare connection "INSERT INTO kvs (key, value) VALUES (?, ?) ON CONFLICT DO UPDATE SET value = excluded.value")
     (sql-prepare connection "DELETE FROM kvs WHERE key = ?"))))
