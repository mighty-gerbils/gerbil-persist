;;;; Key Value Store Interface for sqlite

(import
  :std/db/dbi
  :std/db/sqlite
  :std/db/_sqlite
  :std/iter
  :std/misc/path
  :std/sugar
  :clan/path-config
  :clan/persist/kvs
  :clan/persist/kvs-sql)

(export #t)

(defstruct (KvsSqlite KvsSql)
  (begin-tx-stmt commit-tx-stmt abort-tx-stmt
   read-stmt write-stmt delete-stmt)
  constructor: :init!)

(defmethod {:init! KvsSqlite}
  (lambda (self path (flags (fxior SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE)))
    (def abspath (ensure-absolute-path path persistent-directory))
    (create-directory* (path-parent abspath))
    (def connection (sqlite-open abspath flags))
    (sql-eval connection (string-append
                          "PRAGMA locking_mode = EXCLUSIVE ; "
                          "PRAGMA synchronous = FULL ; "))
    (sql-eval connection (string-append
                          "CREATE TABLE IF NOT EXISTS kvs ( "
                          "key BLOB PRIMARY KEY, "
                          "value BLOB NOT NULL ) "
                          "WITHOUT ROWID"))
    (struct-instance-init!
     self connection
     (sql-prepare connection "BEGIN IMMEDIATE TRANSACTION")
     (sql-prepare connection "COMMIT TRANSACTION")
     (sql-prepare connection "ROLLBACK TRANSACTION")
     (sql-prepare connection "SELECT value FROM kvs WHERE key = ?")
     (sql-prepare connection "INSERT INTO kvs (key, value) VALUES (?, ?) ON CONFLICT DO UPDATE SET value = excluded.value")
     (sql-prepare connection "DELETE FROM kvs WHERE key = ?"))))
