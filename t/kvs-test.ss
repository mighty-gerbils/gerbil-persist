(export kvs-test test-kvs)

(import
  :std/test
  :clan/persist/kvs)

;; Test a Key Value Store interface
(def (test-kvs K)
  (def k1 #u8(1))
  (def k2 #u8(2 3))
  (def v1 (string->bytes "Hello, World!"))
  (def v2 (string->bytes "FOO"))
  {begin-transaction K}
  (check-equal? (values->list {read-key K k1}) [#f #f])
  {write-key K k1 v1}
  {commit-transaction K}
  {begin-transaction K}
  (check-equal? (values->list {read-key K k1}) [v1 #t])
  {write-key K k1 v2}
  {commit-transaction K}
  {begin-transaction K}
  (check-equal? (values->list {read-key K k1}) [v2 #t])
  {delete-key K k1}
  {commit-transaction K}
  (check-equal? (values->list {read-key K k1}) [#f #f])
  (void))

(def kvs-test
  (test-suite "test suite for persist/kvs-sqlite"
    (void)))
