;;;; Key Value Store Interface for Postgres

(import
  :std/db/dbi
  :std/db/postgresql
  :std/db/postgresql-driver
  :std/iter
  :std/misc/path
  :std/sugar
  :clan/path-config
  :clan/persist/kvs
  :clan/persist/kvs-sql)

(defstruct (KvsPostgres KvsSql)
  (begin-tx-stmt commit-tx-stmt abort-tx-stmt
   read-stmt write-stmt delete-stmt)
  constructor: :init!)


(defmethod {:init! KvsPostgres}
  (lambda (self . args)
    (def connection (apply sql-connect args))
    (sql-eval connection (string-append
                          "CREATE TABLE IF NOT EXISTS kvs ( "
                          "key BLOB, "
                          "value BLOB NOT NULL, "
                          "PRIMARY KEY (key)) ;"))
    (struct-instance-init!
     self connection
     (sql-prepare connection "BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE, READ WRITE")
     (sql-prepare connection "COMMIT TRANSACTION")
     (sql-prepare connection "ROLLBACK TRANSACTION")
     (sql-prepare connection "SELECT value FROM kvs WHERE key = ?")
     (sql-prepare connection "INSERT INTO kvs (key, value) VALUES (?, ?) ON CONFLICT DO UPDATE SET value = excluded.value")
     (sql-prepare connection "DELETE FROM kvs WHERE key = ?"))))
