(export kvs-leveldb-test)

(import
  :std/test
  :std/misc/process
  :clan/base :clan/path-config
  :clan/persist/kvs-leveldb
  ./kvs-test)

(def kvs-leveldb-test
  (test-suite "test suite for persist/kvs-leveldb"
    (test-case "Test leveldb"
      (def dbpath (transient-path "t/kvs-leveldb-test.db"))
      (run-process/batch ["rm" "-rf" dbpath])
      (test-kvs (make-KvsLeveldb dbpath)))))
