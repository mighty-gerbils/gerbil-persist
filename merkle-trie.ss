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

(define-type (DigestedTrie. @ [Trie.] Key Height Value .digesting T Step .wrap)
  Digest: (Digesting-Digest .digesting)
  .validate: (.@ Digest .validate)
  .sexp<-: (.@ Digest .sexp<-)
  .marshal: (.@ Digest .marshal)
  .unmarshal: (.@ Digest .marshal)
  .bytes<-: (.@ Digest .bytes<-)
  .<-bytes: (.@ Digest .<-bytes)
  .json<-: (.@ Digest .json<-)
  .<-json: (.@ Digest .<-json)
  Wrapper: {
    .ap: (lambda (v) (digest<-bytes (.call T .bytes<- v) .digesting))
    .unap: invalid .bind: invalid .map: invalid }
  Unstep: =>.+ {(:: @ [] .symmetric)
                .up: (.symmetric branch: (lambda (_ h l r) (.wrap (Branch h l r)))
                                 skip: (lambda (_ h l b c) (.wrap (Skip h l b c))))}
  Path: =>.+ {(:: @ [] .op)
              .up: (let (up (.@ Unstep .up)) (lambda (t path) (.op up t path)))})

(def (DigestedTrie Key Height Value .digesting)
  {(:: @ DigestedTrie.) Key Height Value .digesting
   sexp: `(DigestedTrie ,(.@ Key sexp) ,(.@ Height sexp)
                        ,(.@ Value sexp) ,(Digesting-sexp .digesting))})

(define-type (MerkleTrie. @ [ContentAddressed. Trie.]
                            Key Height Value .wrap .unwrap .refocus .zipper<- Path
                            .digesting .digest<-)
  T: =>.+ { .walk-dependencies:
            (lambda (f t) (match t
                       ((Empty) (void))
                       ((Leaf v) (f Value v))
                       ((Branch _ l r) (f @ l) (f @ r))
                       ((Skip _ _ _ c) (f @ c)))) }
  Digested: {(:: @D [DigestedTrie.]) Key Height Value .digesting}
  .proof<-: ;; : (Path Digest)
  (lambda (trie key)
    (match (.refocus ($Costep -1 key) (.zipper<- trie))
      ([sub . up] (cons sub (.call Path .map .digest<- up)))))
  .validate-proof:
  (lambda (trie-digest sub up)
    (match (.unwrap sub)
      ((Leaf v)
       (validate Value v)
       (let (digest (car ((.@ Digested Path .up) (.call Digested .leaf v) up)))
         (unless (equal? trie-digest digest)
           (let (D (Digesting-Digest .digesting))
             (raise-type-error "Digest doesn't match: " D trie-digest D digest up)))))
      ;; TODO: support negative proofs
      (_ (raise-type-error "No leaf" sub up)))))
(def (MerkleTrie Key: (Key UInt) Height: (Height UInt)
                 Value: (Value Any) Digesting: (.digesting keccak-addressing))
  {(:: @ [MerkleTrie.]) Key Height Value .digesting
   sexp: `(MerkleTrie Key: ,(.@ Key sexp) Height: ,(.@ Height sexp) Value: ,(.@ Value sexp)
                      Digesting: ,(Digesting-sexp .digesting))})
