(export #t)

(import
  :clan/base
  :clan/poo/poo :clan/poo/mop :clan/poo/type :clan/poo/brace :clan/poo/trie :clan/poo/io
  ./content-addressing)

;; TODO: support Value itself being digested!
;; TODO: automatically work recursively on the type functor of which the data structure is a fixed-point.
(.def (DigestedTrie. @ [Trie.] Key Height Value .digesting T)
  sexp: `(DigestedTrie ,(.@ Key sexp) ,(.@ Height sexp) ,(.@ Value sexp) ,(.@ .digesting sexp))
  wrap: (lambda (v) (digest<-bytes (.call T bytes<- v) .digesting))
  unwrap: invalid)
(def (DigestedTrie Key Height Value .digesting)
  {(:: @ DigestedTrie.) Key Height Value .digesting})

(.def (MerkleTrie. @ [ContentAddressed. Trie.]
                     Key Height Value .wrap .unwrap .refocus .zipper<- Path
                     .digesting digest<-)
   sexp: `(MerkleTrie ,(.@ Key sexp) ,(.@ Height sexp) ,(.@ Value sexp))
   T: =>.+ {
     .walk-dependencies:
     (lambda (f t)
       (match t
         ((Empty) (void))
         ((Leaf v) (f Value v))
         ((Branch _ l r) (f @ l) (f @ r))
         ((Skip _ _ _ c) (f @ c))))}
   Digested: {(:: @D [DigestedTrie.]) Key Height Value .digesting}
   Unstep: =>.+ {(:: @U [] .symmetric)
     ;; TODO: implement a variant that recognizes the digest of Empty, to support negative proofs
     .digest: (.symmetric branch: (lambda (_ h l r) (.call Digested .wrap (Branch h l r)))
                          skip: (lambda (_ h hb b c) (.call Digested .wrap (Skip h hb b c))))}
   .proof<-: (lambda (trie key)
              (match (.refocus ($Costep -1 key) (.zipper<- trie))
                ([sub . up] (cons sub (.call Path .map digest<- up)))))
   .validate-proof:
   (lambda (trie-digest sub up ctx)
     (def c [[validate-proof: trie-digest sub up] . ctx])
     (match sub
       ((Leaf v)
        (validate Value v c)
        (let (digest (.call Path .op (.@ Unstep .digest) (.call Digested .leaf v) up))
          (unless (equal? trie-digest digest)
            (type-error c "Digest doesn't match" digest))))
       ;; TODO: support negative proofs
       (_ (type-error c "No leaf")))))
(def (MerkleTrie Key Value)
  {(:: @ [MerkleTrie.]) (Key) (Value)})
