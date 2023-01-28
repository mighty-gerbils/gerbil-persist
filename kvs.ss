;;;; Key Value Store Interface

(import
  :std/db/dbi :std/db/sqlite :std/error :std/sugar :std/misc/list-builder
  :clan/base)

(export #t)

(defstruct (db-error <error>) ())

(def (raise-db-error where . what)
  (raise (make-db-error what [] where)))

(defstruct Kvs
  (connection))

(defmethod {:init! Kvs}
  (lambda (self e) (struct-instance-init! self e)))

(defmethod {close Kvs}
  (lambda (self) {close (Kvs-connection self)}))

;; NB: In the near future, a key value store backed by several remote servers may implement
;; this method by querying its multiple replicas and identifying whichever is correct.
(defmethod {read-decode-check-key Kvs}
  (lambda (self key decode check?)
    (defvalues (bytes present?) {read-key self key})
    (unless present?
      (raise-db-error 'read-decode-check-key 'kvs-key-absent key))
    (def value (decode bytes))
    (unless (check? value)
      (error 'kvs-data-tampering "Database was tampered with" self key))
    value))
