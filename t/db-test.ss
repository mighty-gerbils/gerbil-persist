(export db-test)

(import
  :gerbil/gambit
  :std/assert :std/format
  :std/misc/bytes :std/misc/list :std/misc/number :std/misc/repr
  :std/sugar :std/test
  :clan/concurrency :clan/path-config
  ../db)

(def db-test
  (test-suite "test for persist/db"
    (test-case "open close db twice"
      (prn 1)
      (def c (open-db-connection "testdb"))
      (close-db-connection! c)
      (prn 2)
      (def c2 (open-db-connection "testdb"))
      (close-db-connection! c2))
    (test-case "test-trivial-testdb-put-get"
      (def test-val (random-integer 1000000000))
      (def key (string->bytes "test-key1"))
      (with-db-connection (c-path "testdb")
        (with-committed-tx (tx) (db-put! key (uint->u8vector test-val) tx))
        (check-equal? (u8vector->uint (with-tx (tx) (db-get key tx))) test-val)
        (with-committed-tx (tx) (db-put! key (uint->u8vector (1+ test-val)) tx))
        (check-equal? (u8vector->uint (with-tx (tx) (db-get key tx))) (1+ test-val))))
    (test-case "test-trivial-testdb-commits"
      (def test-base (* 65536 (random-integer 65536)))
      (def n-workers 200)
      (def failures [])
      (def (test-commit i)
        (def key (uint->u8vector (+ i 65536)))
        (def val (string->bytes (number->string (+ test-base i))))
        (try
         (match (current-db-transaction)
           ((DbTransaction connection txid status completion)
            (printf "~s\n" [LEAKED-TX-PARAMETER: i connection txid status])
            (exit 1)
            (push! [LEAKED-TX-PARAMETER: i connection txid status] failures))
           (#f (void)))
         ;; NB: use assertions instead of check, because we're running in a thread.
         (assert! (equal? (current-db-transaction) #f))
         (with-committed-tx (tx) (db-put! key val tx))
         (assert! (equal? (with-tx (tx) (db-get key tx)) val))
         (catch (e)
           (printf "IN TEST ~d: ~a\n" i (error-message e))
           (push! [i (error-message e)] failures))))
      (defvalues (initial-batch-id final-batch-id)
      (with-db-connection (c "testdb")
        (def initial-batch-id (get-batch-id c))
        (parallel-map test-commit (iota n-workers 1))
        (def final-batch-id (get-batch-id c))
        (values initial-batch-id final-batch-id)))
      (printf "initial batch: ~d\nfinal batch: ~a\n" initial-batch-id final-batch-id)
      (for-each (cut printf "~s\n" <>) failures)
      (check (- final-batch-id initial-batch-id) ? (cut <= 1 <> 3))
      (check failures ? null?))))
