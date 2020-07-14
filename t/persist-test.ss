(export persist-test)

(import
  :std/test
  ../persist)

(def persist-test
  (test-suite "test suite for persist/persist"
    (test-case "nop"
      (void))))
