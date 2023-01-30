(export kvs-sqlite-test)

(import
  :std/test
  :clan/base :clan/path-config
  :clan/persist/kvs-sqlite
  ./kvs-test)

(def kvs-sqlite-test
  (test-suite "test suite for persist/kvs-sqlite"
    (test-case "Test sqlite"
      (def dbpath (transient-path "t/kvs-sqlite-test.db"))
      (ignore-errors (delete-file dbpath))
      (test-kvs (make-KvsSqlite dbpath)))))
