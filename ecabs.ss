;; Encrypted Content-Addressed Byte Store
;; TODO: have a competent cryptographer review.
;; (Thanks to Alipha and maroon from IRC libera.chat #crypto for help with the design.)

(import :std/crypto/libcrypto :std/crypto/cipher
        :clan/crypto/keccak)

(defstruct EncryptionContext
  (encrypt ;; symmetric encryption function (cut encrypt (make-cipher e) masterkey <> <>)
   decrypt ;; symmetric decryption function (cut decrypt (make-cipher e) masterkey <> <>)
   digest ;; hash function
   derive-key ;; function to derive key from content hash
   derive-iv)) ;; derived from derive iv from content hash

(defmethod {:init! EncryptionContext}
  (lambda (self encrypt decrypt digest masterkey keysalt valuesalt ivlen)
    (struct-instance-init! self encrypt decrypt digest masterkey keysalt valuesalt ivlen)))

(def (standard-encryption-context
      masterkey
      cipher: (cipher (let (cipher (EVP_aes_256_ctr)) (cut make-cipher cipher)))
      digest: (digest keccak256<-bytes)
      ivlen: (ivlen 16)) ;; length of IV for cipher
  (def key-salt (digest (u8vector-append masterkey (string->bytes "key"))))
  (def iv-salt (digest (u8vector-append masterkey (string->bytes "iv"))))
  (make-EncryptionContext
    (cut encrypt (cipher) masterkey <> <>)
    (cut decrypt (cipher) masterkey <> <>)
    digest
    (lambda (hash) (digest (u8vector-append key-salt hash)))
    (lambda (hash) (subu8vector (digest (u8vector-append iv-salt hash)) 0 ivlen))))

(def (store-bytes kvs crypt-ctx bytes)
  (with ((EncryptionContext encrypt _ digest derive-key derive-iv) crypt-ctx)
    (def hash (digest bytes))
    (def key (derive-key hash))
    (def iv (derive-iv hash))
    (def value (encrypt iv bytes))
    {write-key kvs key value}
    hash))

(def (load-bytes kvs crypt-ctx hash)
  (with ((EncryptionContext _ decrypt digest derive-key derive-iv) crypt-ctx)
    (def key (derive-key hash))
    (def iv (derive-iv hash))
    {read-decode-check-key
     kvs key (cut decrypt iv <>) (lambda (bytes) (equal? hash (digest bytes)))}))
