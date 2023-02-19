(export #t)

(import
  :gerbil/gambit/random
  :std/format :std/misc/list :std/misc/repr
  :std/sugar :std/test :std/text/hex
  :clan/assert
  :clan/concurrency :clan/number :clan/path-config
  :clan/testing
  :clan/poo/object :clan/poo/type :clan/poo/number :clan/poo/trie
  :clan/poo/t/table-testing
  ../db ../content-addressing ../merkle-trie)

(def T (MerkleTrie Value: String))

(def-table-test-accessors T)

(def trie-100 (<-alist al-100-decimal))
(def trie-10-12-57 (<-l '(10 12 57)))
(def trie-4 (<-l '(1 2 3 4)))

(def (check-proof t k v)
  (match (F .proof<- t k)
    ([sub . up]
     (assert-equal! (F .unwrap sub) (Leaf v))
     (F .validate-proof (F .digest<- t) sub up '()))
    (_ (error "foo"))))

(def (merkle-tests T)
  (table-test-case T "merkle trie tests"
    (test-case "simple proof consistent 1"
      (check-proof (<-l '(0 1)) 0 "0"))
    (test-case "simple proof consistent 100"
      (check-proof (F .singleton 100 "100") 100 "100"))
    (def good-merkle-path
      ($Path ($Costep -1 42)
             (map (lambda (x) (BranchStep (hex-decode x)))
                  ["7c79d94b79bf44bc3a8d1f9c1d6f887fad01a94aeaf95060f994ac6f29f2fbd4"
                   "c024a35e4802d3a7e43d7c6e87f687417fc8a9ef1a3a664952151381e58942b1"
                   "8399f332007d0fbacbc5b9eea7fff98295de897e8fd176981ff289e47eef7a5f"
                   "d9b180853f5efbd8632128050ffdadf762e0304740507bbb801a04702353f858"
                   "7b5bbad9ce47c55a0f88dd235c7bf23b7ebc341e0ba59106d2c247a60b9b60f6"
                   "afb6fd06436bc357dfee6f4b33196f2240a685a876e22aa87eec79830560fdf7"
                   "80a28f0ced76060709881876dfc255d2e06f6d0a0cb33c5677c1e0402079900d"])))
    (def bad-merkle-path
      (match good-merkle-path
        (($Path C [s1 s2 s3 s4 s5 s6 s7]) ($Path C [s1 s2 s5 s4 s3 s6 s7])))) ;; swap steps 3 and 5
    (test-case "proof"
      (check-equal? (cdr (F .proof<- trie-100 42)) good-merkle-path))
    (test-case "simple-proof-consistent"
      (check-proof (F .singleton 0 "0") 0 "0"))
    (test-case "simple-proof-consistent-4"
      (check-proof (F .singleton 4 "4") 4 "4"))
    (test-case "simple-proof-consistent-10"
      (check-proof trie-10-12-57 57 "57"))
    (test-case "simple-proof-consistent-24"
      (check-proof trie-4 2 "2"))
    (test-case "proof-inconsistent"
      (check-exception (F .validate-proof (F .digest<- trie-100) (F .leaf 42) good-merkle-path) true))
    (test-case "proof-inconsistent"
      (check-exception (F .validate-proof (F .digest<- trie-100) (F .leaf 42) bad-merkle-path) true))))

(def merkle-trie-test
  (test-suite "integration test for persist/merkle-trie"
    (init-test-random-source!)
    (with-db-connection (c "testdb")
      (table-tests T)
      (merkle-tests T))))
