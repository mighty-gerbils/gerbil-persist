(export kvs-test test-kvs test-kvs-single-write test-kvs-multi-write begin-tx!)

(import
  :std/test
  :clan/persist/kvs)

;; Normalize begin-transaction across backends:
;;   SQL backends return void -> use K directly as the tx handle.
;;   KvsFile returns a KvsFileTx -> use that.
(def (begin-tx! K)
  (let ((tx {begin-transaction K}))
    (if (void? tx) K tx)))

;; Test a KVS backend with one write per transaction.
;; Works for both single-write and multi-write backends.
(def (test-kvs-single-write K)
  (def k1 #u8(1))
  (def k2 #u8(2 3))
  (def v1 (string->bytes "Hello, World!"))
  (def v2 (string->bytes "FOO"))
  (let ((tx (begin-tx! K)))
    (check-equal? (values->list {read-key tx k1}) [#f #f])
    {write-key tx k1 v1}
    {commit-transaction tx})
  (let ((tx (begin-tx! K)))
    (check-equal? (values->list {read-key tx k1}) [v1 #t])
    {write-key tx k1 v2}
    {commit-transaction tx})
  (let ((tx (begin-tx! K)))
    (check-equal? (values->list {read-key tx k1}) [v2 #t])
    {delete-key tx k1}
    {commit-transaction tx})
  (check-equal? (values->list {read-key K k1}) [#f #f])
  (void))

;; Test that a backend supports multiple writes within a single transaction.
;; Only call this for backends that allow multi-write transactions (e.g. SQL).
(def (test-kvs-multi-write K)
  (def k1 #u8(1))
  (def k2 #u8(2 3))
  (def v1 (string->bytes "Hello, World!"))
  (def v2 (string->bytes "FOO"))
  (let ((tx (begin-tx! K)))
    {write-key tx k1 v1}
    {write-key tx k2 v2}
    {commit-transaction tx})
  (let ((tx (begin-tx! K)))
    (check-equal? (values->list {read-key tx k1}) [v1 #t])
    (check-equal? (values->list {read-key tx k2}) [v2 #t])
    {commit-transaction tx})
  (void))

;; Run both single-write and multi-write tests.
;; Use only for backends supporting multi-write transactions.
(def (test-kvs K)
  (test-kvs-single-write K)
  (test-kvs-multi-write K))

(def kvs-test
  (test-suite "test suite for persist/kvs-sqlite"
    (void)))
