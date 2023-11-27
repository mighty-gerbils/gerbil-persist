(export persist-test)

(import
  :std/test
  :clan/poo/type
  :clan/poo/number
  ../db ../persist)

(def persist-test
  (test-suite "test suite for persist/persist"
    (test-case "define-persistent-variable"
      (define-persistent-variable my-string String "my-string" "foo")
      (define-persistent-variable my-num UInt256 "my-num" 42)
      (with-db-connection (c "testdb")
        ;; Delete the persistent variables, in case the testdb is dirty
        (with-tx (tx)
          (for-each (lambda (k) (db-delete! (ensure-db-key k)))
                    ["my-string" "my-num"]))
        (check (my-string) => "foo")
        (set! (my-string) "bar")
        (check (my-string) => "bar")
        (check (my-num) => 42)
        (set! (my-num) 69)
        (check (my-num) => 69))
      (with-db-connection (c "testdb")
        (check (my-string) => "bar")
        (set! (my-string) "baz")
        (check (my-string) => "baz")
        (check (my-num) => 69)
        (set! (my-num) 100)
        (check (my-num) => 100))
      ;; Reset for next tests
      (with-db-connection (c "testdb")
        (set! (my-string) "foo")
        (set! (my-num) 42)))))
