(export ebs-test)

(import
  :std/sugar :std/test :std/text/hex
  :clan/base :clan/path-config
  ../kvs ../kvs-sqlite ../ebs)

(def ebs-test
  (test-suite "test suite for ebs (encrypted byte store)"
    (test-case "Test encrypted sqlite kvs"
      (def masterkey (string->bytes "Hello, World!\n+-0123456789ABCDEF"))
      (def dbpath (transient-path "t/kvs-sqlite-test.db"))
      (ignore-errors (delete-file dbpath))
      (def K (make-KvsSqlite dbpath))
      (def C (standard-encryption-context masterkey))
      (def data (string->bytes "This is a test. I repeat. This is a test."))
      (def intent (string->bytes "let there be light"))
      (def h ((EncryptionContext-digest C) data))
      {begin-transaction K}
      (check-exception (load-content-addressed-bytes K C h) db-error?)
      (check-equal? (store-content-addressed-bytes K C data) h)
      {commit-transaction K}
      {begin-transaction K}
      (check-equal? (load-content-addressed-bytes K C h) data)
      (check-exception (load-intent-addressed-bytes K C intent) db-error?)
      (check-equal? (store-intent-addressed-bytes K C intent data) (void))
      {commit-transaction K}
      {begin-transaction K}
      (check-equal? (load-intent-addressed-bytes K C intent) data)
      {commit-transaction K}
      (void))))
