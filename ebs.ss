;; Encrypted Byte Store
;; TODO: have a competent cryptographer review.
;; (Thanks to Alipha and maroon from IRC libera.chat #crypto for help with the design.)

(import :std/crypto/libcrypto :std/crypto/cipher
        :clan/crypto/keccak)

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
  (def ca-key-salt (digest (u8vector-append masterkey (string->bytes "content-addressed-key"))))
  (def ca-iv-salt (digest (u8vector-append masterkey (string->bytes "content-addressed-iv"))))
  (def ia-key-salt (digest (u8vector-append masterkey (string->bytes "intent-addressed-key"))))
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

;; Store bytes at an intent identified by some hash
(def (store-intent-addressed-bytes kvs crypt-ctx hash bytes)
  (with ((EncryptionContext encrypt _ iv-len _ _ derive-ia-key) crypt-ctx)
    (def key (derive-ia-key hash))
    (def iv (random-bytes iv-len))
    (def value (encrypt iv bytes))
    {write-key kvs key (append-bytes iv value)}
    hash))

(def (load-intent-addressed-bytes kvs crypt-ctx hash (valid? true))
  (with ((EncryptionContext _ decrypt iv-len _ _ _ derive-ia-key) crypt-ctx)
    (def key (derive-ia-key hash))
    (def iv (derive-iv hash))
    {read-decode-check-key
     kvs key (lambda (iv+ct)
               (def iv (subu8vector iv+ct 0 iv-len))
               (def ciphertext (subu8vector iv+ct iv-len (u8vector-length iv+ct)))
               (decrypt iv ciphertext))
     valid?}))
