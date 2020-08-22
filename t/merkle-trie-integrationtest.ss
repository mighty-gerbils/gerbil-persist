(export db-integrationtest)

(import
  :gerbil/gambit/random
  :std/sugar :std/test
  :std/format :std/misc/list :std/misc/repr
  :clan/concurrency :clan/number :clan/path-config
  :clan/poo/type
  :clan/poo/t/table-testing
  ../db ../merkle-trie)

(.def (T @ [MerkleTrie])
   Key: UInt256 Value: String)

(def (merkle-tests T)
  (table-test-case T "merkle trie tests"
    (def (check-proof t k v)
      (match (F .proof<- t k)
        ([sub . up]
         (assert-equal! (F .unwrap sub) (Leaf v))
         (F .validate-proof (F .digest<- t) sub up '()))
        (_ (error "foo"))))
    (test-case "simple proof consistent 100"
      (check-proof (F .singleton 100 "100") 100 "100"))
    (test-case "simple_proof_consistent_1"
      (check-proof (<-alist '((0 . "0") (1 . "1"))) 0 "0"))
    (test-case "simple_proof_consistent_1"
      (check-proof (<-alist '((0 . "0") (1 . "1"))) 0 "0"))
    (def good-merkle-path
      (cons ($Costep -1 42)
            (map (lambda (x) (BranchStep (hex-decode x)))
                 ["064880959049e1ee770ac5669c3960ebe0121394b82966503f91439bc545caa4"
                  "b3d80cf4680db145cafc56adfda19cd3f745cdbd1d0294cffe7491cc49b54320"
                  "e48546b788ccbca97760865fe0e901fbc28ad8689cbdffadc9fe80d014207651"
                  "faaea838f3c3dcb79244c307ba41e0cf36cfe8104895239bad3df50a041601bd"
                  "a7358ee547e1cf2ba6d90bddea49c1bfa44686c7a8103e4816794ebc23b365a5"
                  "d09db87e7553ac9ed1760c94ef8a5e2a85683051d6906c54fc603298357998a0"
                  "e6520b16a86ad925aacd94247978f64ebea0d7e6d530247dc612c7a3897d5eb1"])))
    (def bad-merkle-path
      (match good-merkle-path
        ((cons C [s1 s2 s3 s4 s5 s6 s7]) (cons C [s1 s2 s5 s4 s3 s6 s7])))) ;; swap steps 3 and 5
    (test-case "proof"
      (check-equal? (F .proof<- trie-100 42) (cons (F .leaf "42") good-merkle-path)))
    (test-case "simple-proof-consistent"
      (check-proof (F .singleton 0 "0") 0 "0"))
    (test-case "simple-proof-consistent-4"
      (check-proof (F .singleton 4 "4") 4 "4"))
    (test-case "simple-proof-consistent-10"
      (check-proof trie-10-12-57 57 "57"))
    (test-case "simple-proof-consistent-24"
      (check-proof trie-4 2 "2"))
    (test-case "proof-inconsistent"
      (check-exception (F .validate-proof (F .digest<- trie-100) (F .leaf 42) good-merkle-path)))
    (test-case "proof-inconsistent"
      (check-exception (F .validate-proof (F .digest<- trie-100) (F .leaf 42) bad-merkle-path)))))

(def merkle-trie-integrationtest
  (test-suite "integration test for persist/merkle-trie"
    (init-test-random-source!)
    (with-db-connection (c (run-path "testdb"))
      (table-tests T)
      (merkle-tests T))))
