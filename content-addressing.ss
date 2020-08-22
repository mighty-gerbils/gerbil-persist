;; Content Addressing
(export #t)
(import
  (for-syntax :clan/syntax)
  :gerbil/gambit/bytes :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/lazy :std/misc/completion :std/misc/hash :std/sugar
  :clan/base :clan/concurrency :clan/string
  :clan/poo/poo :clan/poo/mop :clan/poo/io :clan/poo/type :clan/poo/brace
  :clan/crypto/keccak
  ./db ./db-queue ./persist)

;; Should the structures belo POO traits instead? Probably not until POO traits are efficient, too!
;; Should that be called a DigestingContext and ContentAddressingContext,
;; to be provided statically or dynamically to make ContentAddressable objects?
;; Note that in a given application, a *same* object may be considered from the point of view
;; of *several* Digesting contexts, because it may be tracked similtaneously in multiple
;; blockchains or registries.
;; But usually, there content-addressing is tied to a specific database,
;; and uses a fixed digesting context that is extended with extra caches.

(defstruct Digesting
  (sexp ;; : SExp ;; how to print it.
   Digest ;; : Type ;; some fixed-size Bytes.
   digest)) ;; : Digest <- Bytes

(defstruct (ContentAddressing Digesting)
  (key-prefix ;; : Bytes
   mutex)) ;; : Mutex

;; : ContentAddressing
(def keccak-addressing
  (make-ContentAddressing
   'keccak-addressing
   (BytesN 32)
   keccak256<-bytes
   (string->bytes "K2")
   (make-mutex 'k2cas)))

;; : (Parameter ContentAddressing)
(def current-content-addressing (make-parameter keccak-addressing))

;; : Digest <- Bytes ?Digesting
(def (digest<-bytes bytes (digesting (current-content-addressing)))
  ((Digesting-digest digesting) bytes))

;; : Digest <- T:Type T ?Digesting
(def (digest<- type value (digesting (current-content-addressing)))
  (digest<-bytes (bytes<- type value) digesting))

;; : Digest <- String ?Digesting
(def (digest<-string string (digesting (current-content-addressing)))
  (digest<-bytes (string->bytes string) digesting))

;; trait for digestability in a given content-addressing context
(.def (Digestable @ [] .bytes<- .digesting)
  .digest<-: (lambda (v (digesting .digesting)) (digest<-bytes (.bytes<- v) digesting)))

;; Non-functor function
(.def (DigestWrapper^ @ [])
  .tap: (lambda (t) (Digesting-Digest (.@ t .digesting)))
  .ap^: (cut .call <> .digest<- <>)
  .unap^: invalid
  .marshal^: (lambda (t v port) (marshal (Digesting-Digest (.@ t .digesting)) v port))
  .unmarshal^: (lambda (t port) (unmarshal (Digesting-Digest (.@ t .digesting)) port)))

;; CAVEAT EMPTOR: This trait statically but *lazily* captures
;; the dynamic current-content-addressing in this interface at time of first reference.
;; This allows you to define all your interfaces independently from which digest function will be used,
;; but a given poo interface should be used in one context only, they should be initialized together,
;; you may want to statically clone and override in some cases, etc.
(.def (CurrentDigesting @ []) ;; inherit: Digestable
  .digesting: (current-content-addressing))

;; : Bytes <- Digest ?ContentAddressing
(def (content-addressing-key digest (content-addressing (current-content-addressing)))
  (u8vector-append (ContentAddressing-key-prefix content-addressing) digest))

(.def (ContentAddressable @ [] sexp .digesting .digest<- .<-bytes .bytes<-)
  ;; CAVEAT EMPTOR: The application developers must ensure there are no collisions
  ;; with respect to sexp for types stored in a given content-addressable context.
  .content-cache: (make-hash-table weak-values: #t)

  ;; @ <- Digest TX
  .<-digest:
  (lambda (digest tx)
    ;; TODO: figure out what are or aren't Gambit's guarantees regarding
    ;; concurrent access to a table.
    ;; Concurrency, reentrance, etc., may cause issues here, but a mutex doesn't seem composable.
    ;; Some kind of transactional memory may be required, at which point,
    ;; should the caching, decoding and transacting service be moved "upstream"
    ;; into the database thread and/or mutex?
    (hash-ensure-ref .content-cache digest
                     (cut .<-bytes (db-get (content-addressing-key digest .digesting) tx))))

  .make-persistent:
  (lambda (x tx)
    (def b (.bytes<- x))
    (def d (digest<-bytes b .digesting))
    (def k (content-addressing-key (d .digesting)))
    (unless (db-key? k tx)
      (make-dependencies-persistent @ x tx)
      (db-put! k b tx))))

(defstruct DV ;; (forall T:Type Type)
  (type ;; : T:Type
   value ;; : (Lazy T)
   digest ;; : (Lazy Digest)
   persisted?)) ;; : Bool

(def (value<-dv dv) (force (DV-value dv)))
(defrule (dv t x) (let (t t) (DV t (lazy x) (lazy (digest<- t x)) #f)))
(def (digest<-dv dv) (force (DV-digest dv)))
(def (dv<-digest t d) (DV t (lazy (.call t .<-digest d)) (lazy d) #t))

;; ContentAddressed
(.def (ContentAddressed. @ [ContentAddressable] T .digesting)
  sexp: `(ContentAddressed ,(.@ T sexp))
  .validate:
  (lambda (dv (ctx '()))
    (def c [[validating: dv] . ctx])
    (unless (DV? dv) (type-error c "not a DV"))
    (match (std/lazy#&lazy-e (DV-value dv))
      (['resolved . v]
       (validate T v c)
       (match (std/lazy#&lazy-e (DV-digest dv))
         (['resolved . d]
          (unless (equal? d (digest<- T v .digesting)) (type-error c "digest does not match")))
         (_ (void))))
      (_ (void))))
  .Digest: (Digesting-Digest .digesting)
  .bytes<-: digest<-dv
  .<-bytes: (cut dv<-digest @ <>)
  .digest<-: .<-bytes ;; don't double-digest!
  .marshal: (lambda (dv port) (marshal .Digest (digest<-dv dv) port))
  .unmarshal: (lambda (port) (.<-bytes (unmarshal .Digest port)))
  .wrap: (lambda (v) (dv T v))
  .unwrap: value<-dv
  .make-persistent:
  (lambda (dv tx)
    (unless (DV-persisted? dv)
      (let* ((d (digest<-dv dv))
             (k (content-addressing-key d .digesting)))
        (unless (db-key? k tx)
          (let (v (value<-dv dv))
            (make-dependencies-persistent T v tx)
            (db-put! k (bytes<- T v) tx)))))))

(def (ContentAddressed t) {(:: @ ContentAddressed.) t})
