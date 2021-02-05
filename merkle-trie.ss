(export #t)

(import
  :clan/base
  :clan/poo/object :clan/poo/mop :clan/poo/type :clan/poo/brace :clan/poo/io
  :clan/poo/trie :clan/poo/number
  ./content-addressing)

;; TODO: support Value itself being digested! Pass around bytes, not values?
;; TODO: automatically work recursively on a descriptor for the "open recursion scheme" type functor
;; of which the data structure is a fixed-point.
;; TODO: support negative proofs. Specially detect empty digest?
;; TODO: support remember the current skip info in DigestedTrie, so you can properly simulate
;; .make-branch and .make-skip when re-digesting a trie with a leaf removed.

(.def (DigestedTrie. @ [Trie.] Key Height Value .digesting T Step .wrap)
  sexp: `(DigestedTrie ,(.@ Key sexp) ,(.@ Height sexp) ,(.@ Value sexp) ,(Digesting-sexp .digesting))
  Digest: (Digesting-Digest .digesting)
  validate: (.@ Digest validate)
  .sexp<-: (.@ Digest .sexp<-)
  .marshal: (.@ Digest .marshal)
  .unmarshal: (.@ Digest .marshal)
  .bytes<-: (.@ Digest .bytes<-)
  .<-bytes: (.@ Digest .<-bytes)
  .json<-: (.@ Digest .json<-)
  .<-json: (.@ Digest .<-json)
  Wrapper: {
    .ap: (lambda (v) (digest<-bytes (.call T .bytes<- v) .digesting))
    .unap: invalid .bind: invalid .map: invalid}
  Unstep: =>.+ {(:: @ [] .symmetric)
                .up: (.symmetric branch: (lambda (_ h l r) (.wrap (Branch h l r)))
                                 skip: (lambda (_ h l b c) (.wrap (Skip h l b c))))}
  Path: =>.+ {(:: @ [] .op)
              .up: (let (up (.@ Unstep .up)) (lambda (t path) (.op up t path)))})

(def (DigestedTrie Key Height Value .digesting)
  {(:: @ DigestedTrie.) Key Height Value .digesting})

(.def (MerkleTrie. @ [ContentAddressed. Trie.]
                     Key Height Value .wrap .unwrap .refocus .zipper<- Path
                     .digesting .digest<-)
   sexp: `(MerkleTrie Key: ,(.@ Key sexp) Height: ,(.@ Height sexp) Value: ,(.@ Value sexp)
                      Digesting: ,(Digesting-sexp .digesting))
   T: =>.+ { .walk-dependencies:
               (lambda (f t) (match t
                          ((Empty) (void))
                          ((Leaf v) (f Value v))
                          ((Branch _ l r) (f @ l) (f @ r))
                          ((Skip _ _ _ c) (f @ c)))) }
   Digested: {(:: @D [DigestedTrie.]) Key Height Value .digesting}
   .proof<-: (lambda (trie key)
              (match (.refocus ($Costep -1 key) (.zipper<- trie))
                ([sub . up] (cons sub (.call Path .map .digest<- up)))))
   .validate-proof:
   (lambda (trie-digest sub up ctx)
     (def c [[validate-proof: trie-digest sub up] . ctx])
     (match (.unwrap sub)
       ((Leaf v)
        (validate Value v c)
        (let (digest (car ((.@ Digested Path .up) (.call Digested .leaf v) up)))
          (unless (equal? trie-digest digest)
            (let (D (Digesting-Digest .digesting))
              (type-error c "Digest doesn't match: " D trie-digest D digest up)))))
       ;; TODO: support negative proofs
       (_ (type-error c "No leaf")))))
(def (MerkleTrie Key: (Key Nat) Height: (Height Nat)
                 Value: (Value Any) Digesting: (.digesting keccak-addressing))
  {(:: @ [MerkleTrie.]) Key Height Value .digesting})
