;; Encrypted Byte Store
;; Thanks to Alipha and maroon from IRC libera.chat #crypto for help with the design.
;; TODO: Merge context with that from content-addressing.ss
;; TODO: Optionally chunk data over some maximum size, e.g. 64KB, 4KB or 1KB.
;; TODO: Optionally pad data under some minimum size. e.g. 32B, 1KB or 4KB.
;; TODO: Have a competent cryptographer review.

(import :std/crypto/libcrypto :std/crypto/cipher :std/crypto/etc
        :clan/crypto/keccak)

(export #t)

(defstruct EncryptionContext
  (encrypt ;; Bytes <- IV Bytes ;; symmetric encryption (cut encrypt (make-cipher e) masterkey <> <>)
   decrypt ;; Bytes <- IV Bytes ;; symmetric decryption (cut decrypt (make-cipher e) masterkey <> <>)
   iv-len ;; Integer ;; length of an IV (initialization vector)
   digest ;; Hash <- Bytes ;; compute hash from bytes
   derive-ca-key ;; Bytes <- Hash ;; derive db key from content hash
   derive-ca-iv ;; IV <- Hash ;; derive iv from content hash
   derive-ia-key)) ;; Bytes <- Hash ;; derive db key from intent hash

(defmethod {:init! EncryptionContext}
  (lambda (self encrypt decrypt digest masterkey keysalt valuesalt ivlen)
    (struct-instance-init! self encrypt decrypt digest masterkey keysalt valuesalt ivlen)))

(def (standard-encryption-context
      masterkey
      cipher: (cipher (let (cipher (EVP_aes_256_ctr)) (cut make-cipher cipher)))
      digest: (digest keccak256<-bytes)
      iv-len: (iv-len 16)) ;; length of IV for cipher
  (def (make-salt str) (digest (u8vector-append masterkey (string->bytes str))))
  (def ca-key-salt (make-salt "content-addressed-key"))
  (def ca-iv-salt (make-salt "content-addressed-initialization-vector"))
  (def ia-key-salt (make-salt "intent-addressed-key"))
  (make-EncryptionContext
    (cut encrypt (cipher) masterkey <> <>)
    (cut decrypt (cipher) masterkey <> <>)
    iv-len
    digest
    (lambda (hash) (digest (u8vector-append ca-key-salt hash)))
    (lambda (hash) (subu8vector (digest (u8vector-append ca-iv-salt hash)) 0 iv-len))
    (lambda (hash) (digest (u8vector-append ia-key-salt hash)))))

(def (store-content-addressed-bytes kvs crypt-ctx bytes)
  (with ((EncryptionContext encrypt _ _ digest derive-ca-key derive-ca-iv _) crypt-ctx)
    (def hash (digest bytes))
    (def key (derive-ca-key hash))
    (def iv (derive-ca-iv hash))
    (def value (encrypt iv bytes))
    {write-key kvs key value}
    hash))

(def (load-content-addressed-bytes kvs crypt-ctx hash)
  (with ((EncryptionContext _ decrypt _ digest derive-ca-key derive-ca-iv _) crypt-ctx)
    (def key (derive-ca-key hash))
    (def iv (derive-ca-iv hash))
    {read-decode-check-key
     kvs key (cut decrypt iv <>) (lambda (bytes) (equal? hash (digest bytes)))}))

;; Store bytes at an intent identified by some hash or other u8vector
(def (store-intent-addressed-bytes kvs crypt-ctx intent bytes)
  (with ((EncryptionContext encrypt _ iv-len _ _ _ derive-ia-key) crypt-ctx)
    (def key (derive-ia-key intent))
    (def iv (random-bytes iv-len))
    (def value (encrypt iv bytes))
    {write-key kvs key (u8vector-append iv value)}))

(def (load-intent-addressed-bytes kvs crypt-ctx intent (valid? true))
  (with ((EncryptionContext _ decrypt iv-len _ _ _ derive-ia-key) crypt-ctx)
    (def key (derive-ia-key intent))
    {read-decode-check-key
     kvs key (lambda (iv+ct)
               (def iv (subu8vector iv+ct 0 iv-len))
               (def ciphertext (subu8vector iv+ct iv-len (u8vector-length iv+ct)))
               (decrypt iv ciphertext))
     valid?}))
