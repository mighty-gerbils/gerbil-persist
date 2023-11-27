;;;; Key Value Store for SQL in general (to be specialized by SQLite, PostgreSQL, etc.)

(import
  :std/db/dbi
  :std/iter
  :std/misc/path
  :std/sugar
  :clan/path-config
  :clan/persist/kvs)

(export #t)

(defstruct (KvsSql Kvs)
  (begin-tx-stmt commit-tx-stmt abort-tx-stmt
   read-stmt write-stmt delete-stmt)
  constructor: :init!)

(defmethod {:init! KvsSql}
  (lambda (self connection begin-tx-stmt commit-tx-stmt abort-tx-stmt read-stmt write-stmt delete-stmt)
    (struct-instance-init! self connection begin-tx-stmt commit-tx-stmt abort-tx-stmt read-stmt write-stmt delete-stmt)))

(defmethod {begin-transaction KvsSql} (lambda (self) (sql-exec (KvsSql-begin-tx-stmt self))))
(defmethod {abort-transaction KvsSql} (lambda (self) (sql-exec (KvsSql-abort-tx-stmt self))))
(defmethod {commit-transaction KvsSql} (lambda (self) (sql-exec (KvsSql-commit-tx-stmt self))))

(defrule (with-statement (var stmt args ...) body ...)
  (let ((var stmt))
    (try {bind var args ...} body ...
      (finally (sql-reset/clear stmt)))))

(defmethod {read-key KvsSql}
  (lambda (K key)
    (with-statement (s (KvsSql-read-stmt K) key)
      (match {query-fetch s}
        ((eq? #!void) (values {query-row s} #t))
        ((eq? iter-end) (values #f #f))))))

(defmethod {write-key KvsSql}
  (lambda (K k v)
    (with-statement (s (KvsSql-write-stmt K) k v)
      {exec s})))

(defmethod {delete-key KvsSql}
  (lambda (K k)
    (with-statement (s (KvsSql-delete-stmt K) k)
      {exec s})))
