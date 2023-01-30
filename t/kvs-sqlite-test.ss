(export kvs-sqlite-test)

(import
  :std/sugar :std/test :std/text/hex
  :clan/base :clan/path-config
  :clan/persist/kvs-sqlite :clan/persist/kvs)

;; Test a Key Value Store interface
(def (test-kvs K)
  (def k1 #u8(1))
  (def k2 #u8(2 3))
  (def v1 (string->bytes "Hello, World!"))
  (def v2 (string->bytes "FOO"))
  (check-equal? (values->list {read-key K k1}) [#f #f])
  {write-key K k1 v1}
  (check-equal? (values->list {read-key K k1}) [v1 #t])
  {write-key K k1 v2}
  (check-equal? (values->list {read-key K k1}) [v2 #t])
  {delete-key K k1}
  (check-equal? (values->list {read-key K k1}) [#f #f])
  (void))

(def kvs-sqlite-test
  (test-suite "test suite for persist/kvs-sqlite"
    (test-case "Test sqlite"
      (def dbpath (transient-path "t/kvs-sqlite-test.db"))
      (ignore-errors (delete-file dbpath))
      (test-kvs (make-KvsSqlite dbpath)))))
