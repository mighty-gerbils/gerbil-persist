;; Persisting Data
(export #t)
(import
  (for-syntax :clan/syntax)
  :gerbil/gambit/bytes :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/misc/completion :std/misc/hash :std/sugar
  :clan/base :clan/concurrency :clan/string
  :clan/poo/poo :clan/poo/mop :clan/poo/io :clan/poo/type
  ./db ./db-queue)

(.defgeneric (walk-dependencies type f x) ;; Unit <- 'a:Type (Unit <- 'b:Type 'b) 'a
   slot: .walk-dependencies default: void)

;; Unit <- 'a:Type 'a TX
(def (make-dependencies-persistent type x tx)
  (walk-dependencies type (cut make-persistent <> <> tx) x))

;; Unit <- 'a:Type 'a TX
(.defgeneric (make-persistent type x tx)
   slot: .make-persistent default: void)

(.def (Port @ Type.) sexp: 'Port .element?: port?)
(.def (Thread @ Type.) sexp: 'Thread .element?: thread?)
(.def (Completion @ Type.) sexp: 'Completion .element?: completion?)

(.def (TX @ Type.)
   sexp: 'TX
   .element?: DbTransaction?)

;; Persistent objects, whether passive data or activities.
(.def (Persistent. @ Type.
       sexp ;; : Any
       ;; Prefix for keys in database. In a relational DB, that would be the name of the table.
       key-prefix ;; : u8vector
       ;; Type descriptor for keys (to be serialized as DB key)
       Key ;; : Type
       ;; Type descriptor for the persistent state
       State ;; : Type ;; states
       ;; Internal method to re-create the data object from
       ;; (1) the key,
       ;; (2) a function to persist the state,
       ;; (3) the initial state (whether a default state or one read from the database),
       ;; (4) a current transaction context in which the initial state was read.
       ;; The type must provide this method, but users won't use it directly:
       ;; they will call the read method, that will indirectly call make-activity with proper arguments.
       ;; : @ <- Key (Unit <- State TX) State TX
       .restore)
  restore: (validate (Fun @ <- Key (Fun Unit <- State TX) State TX) .restore)

  ;; Internal table of objects that have already been loaded from database.
  ;;loaded:: (Table @ <- Key)
  loaded: (make-hash-table) ;; weak-values: #t

  ;; Internal function that associates a key in the key-value store to a user-level key object of type Key.
  ;; : Bytes <- Key
  db-key<-: (lambda (key) (u8vector-append key-prefix (bytes<- Key key)))

  ;; Internal function that given (1) a db-key (as returned by the function above),
  ;; (2) a current state of type State, and a (3) current transaction context,
  ;; will save said current state in the current transaction.
  ;; Note that this modification will only be committed with the transaction, and
  ;; the activity will have to either synchronously commit-transaction if it owns the transaction,
  ;; or asynchronously call sync-transaction if it doesn't,
  ;; before it may assume the state being committed,
  ;; and start sending according messages to external systems and notifying the user it's done
  ;; (though if transactions have high latency, it might optimistically notify the user
  ;; that the change is underway).
  ;; : (Fun Unit <- Bytes State TX)
  saving: (lambda (db-key state tx)
            (make-dependencies-persistent State state tx)
            (db-put! db-key (bytes<- State state) tx))
  ;; Internal function that given a key returns a default state to associate with that key
  ;; when no state is found in the database.
  ;; Not all activities have a default state, and the default method will just raise an error.
  ;; : (Fun State <- Key)
  make-default-state: (lambda (key) ;; override this method to provide a default state
                        (error "Failed to load key" sexp key))

  ;; Internal function that given (1) a db-key (as returned by the function above),
  ;; (2) a current state of type State, and (3) a current transaction context [TODO: no TX?]
  ;; will restore the object and register it as loaded.
  ;; Note that this will only be committed with the transaction, and the activity will have to
  ;; either synchronously commit-transaction if it owns the transaction, or asynchronously call
  ;; sync-transaction if it doesn't, before it may assume the state being committed.
  ;; : (Fun @ <- Key State TX)
  resume: (validate (Fun @ <- Key State TX) .resume)
  .resume:
  (lambda (key state tx)
    (def db-key (db-key<- key))
    (when (hash-key? loaded db-key)
      (error "persistent activity already resumed" sexp key))
    (def object (restore key (cut saving db-key <> <>) state tx))
    (hash-put! loaded db-key object)
    object)

  ;; Internal function to resume an object from the database given a key and a transaction,
  ;; assuming the object wasn't loaded yet.
  ;; : (Fun @ <- Bytes Key TX)
  resume-from-db:
  (lambda (db-key key tx)
    (def state
      (cond
       ((db-get db-key tx) => (cut <-bytes State <>))
       (else (make-default-state key))))
    (resume key state tx))

  ;; Function to create a new activity (1) associated to the given key,
  ;; (2) the state of which will be computed by the given initialization function
  ;; (that takes the saving function as argument), (3) in the context of the given transaction.
  ;; Note that any modification will only be committed with the transaction, and
  ;; the init function does not own the transaction and thus will have to call sync-transaction
  ;; or wait for some message by someone that does before it may assume the initial state is committed,
  ;; and start sending according messages to external systems. Similarly, the creating context
  ;; must eventually commit, but any part of it that wants to message based on the activity
  ;; has to sync-transaction to wait for it being saved.
  ;; Also, proper mutual exclusion must be used to ensure only one piece of code
  ;; may attempt create to create an activity with the given key at any point in time.
  make: (validate (Fun @ <- Key (Fun State <- (Fun Unit <- State TX) TX) TX) .make)
  .make:
  (lambda (key init tx)
    (def db-key (db-key<- key))
    (when (db-key? db-key tx)
      (error "persistent activity already created" sexp (sexp<- Key key)))
    (def state (init (cut saving db-key <> <>) tx))
    (resume key state tx)))

;; Persistent Data has no activity of its own,
;; and can be synchronously owned or asynchronously borrowed by persistent activities,
;; that will provide a transaction as a context to read of modify the data.
;; In case they may be borrowed, they must provide some mutual exclusion mechanism
;; that the borrowing activity will use to ensure data consistency.
(.def (PersistentData @ Persistent.
       Key loaded resume-from-db db-key<-)
  ;; Read the object from its key, given a context.
  ;; For activities, this is an internal function that should only be called via get.
  ;; For passive data, this is a function that borrowers may use after they ensure mutual exclusion.
  ;; For those kinds of objects where it makes sense, this may create a default activity.
  ;; Clients of this code must use proper mutual exclusion so there are no concurrent calls to get.
  ;; Get may indirectly call resume if the object is in the database, and make-default-state if not.
  get: (validate (Fun @ <- Key TX) .get)
  .get:
  (lambda (key tx)
    (def db-key (db-key<- key))
    (or (hash-get loaded db-key) ;; use the db-key as key so we get the correct equality
        (resume-from-db db-key key tx))))

;; Persistent activities compute independently from each other;
;; they may create transactions when they need to and borrow persistent data;
;; they may synchronize to I/O (including the DB) though outside transactions.
;; Activities communicate with each other using asynchronous messages.
(.def (PersistentActivity @ Persistent.
       Key loaded resume-from-db db-key<-)
  ;; Get the activity by its key.
  ;; No transaction is provided: the activity will make its own if needed.
  <-key: (validate (Fun @ <- Key) .<-key)
  .<-key:
  (lambda (key)
    (def db-key (db-key<- key))
    (or (hash-get loaded db-key) ;; use the db-key as key so we get the correct equality
        (with-tx (tx) (resume-from-db db-key key tx)))))

(defstruct persistent-cell (mx datum save!))

(def (call-with-persistent-cell cell f)
  (with-lock (persistent-cell-mx cell)
    (f (fun (with-cell accessor tx)
         (def (get-state)
           (assert! (eq? (DbTransaction-status tx) 'open))
           (persistent-cell-datum cell))
         (def (set-state! new-state)
           ((persistent-cell-save! cell) new-state tx)
           (set! (persistent-cell-datum cell) new-state))
         (accessor get-state set-state!)))))

;; Persistent actor that has a persistent queue
(.def (PersistentQueueActor @ PersistentActivity
       Key State sexp <-key db-key<-
       ;; type of messages sent to the actor
       Message ;; : Type
       ;; function to process a message
       process) ;; : <- Message (State <-) (<- State) TX
  .restore: ;; Provide the interface function declared above.
  (lambda (key save! state tx)
    (def name [sexp (sexp<- Key key)])
    (def (get-state) state)
    (def (set-state! new-state) (save! new-state tx) (set! state new-state))
    (def (process-bytes msg tx)
      (def message (<-bytes Message msg))
      ;;(DBG process: (sexp<- Key key) (sexp<- State state) (sexp<- Message message))
      (process message get-state set-state! tx))
    (def qkey (u8vector-append (db-key<- key) #u8(81))) ;; 81 is ASCII for #\Q
    (def q (DbQueue-restore name qkey process))
    (cons q get-state))

  ;; Send a message to a persistent actor.
  ;; NB: to avoid redundant (de)serialization, use content-addressing of objects
  ;; to share cached values loaded from the database.
  ;; : <- Message TX
  send:
  (lambda (key message tx)
    (DbQueue-send! (car (<-key key)) (bytes<- Message message) tx))

  ;; : State <- Key
  read:
  (lambda (k) ((cdr (<-key k)))))


;; Persistent actor that has a transaction at every request.
;; Two functions are called: f within the request, k outside of it, both in the context of the actor thread.
;; IMPORTANT: messages sent to the actor MUST be deterministically determined by other persistent data,
;; and idempotent in their effects; they must be re-sent until the desired effect is observed,
;; in case the process is halted before the message was fully processed.
;; Sometimes, you may have to pre-allocate a ticket/nonce/serial-number, save it,
;; so that you can feed the actor an idempotent message.
(.def (PersistentActor @ [Thread PersistentActivity]
       Key State sexp <-key)
  .restore: ;; Provide the interface function declared above.
  (lambda (key save! state tx)
    (def name [sexp (sexp<- Key key)])
    (def (get-state) state)
    (def (set-state! new-state) (save! new-state tx) (set! state new-state))
    (def (process msg)
      ;;(DBG process: name (sexp<- State state) msg)
      (match msg
        ([Transform: f k]
         (call/values (lambda () (with-tx (tx) (f get-state set-state! tx))) k))))
    (def thread
      (without-tx
        (spawn/name/logged name (fun (make-persistent-actor) (while #t (process (thread-receive)))))))
    (set! (thread-specific thread) get-state)
    thread)

  ;; Run an asynchronous action (1) on the actor with given key, as given by
  ;; (2) a function that takes the current state as a parameter as well as
  ;; a function that sets the state to a new value, in (3) a given transaction context.
  ;; The function will be run asynchronously in the context of the actor,
  ;; and its result will be discarded. To return a value to the caller,
  ;; you must explicitly use a completion, or use the action method below, that does.
  ;; After all actions with a given tx are run, the sync method must be called.
  ;; If the tx is #f then a new transaction will be created in the actor's context
  ;; and synchronized; the action function may then use after-commit to send notifications.
  ;; : Unit <- Key (Unit <- State (<- State)) TX
  async-action:
  (lambda (key k)
    (thread-send (<-key key) [Transform: k void]))

  ;; Run a synchronous action (1) on the actor with given key, as given by
  ;; (2) a function that takes the current state as a parameter as well as
  ;; a function that sets the state to a new value, in (3) a given transaction context.
  ;; The function will be run asynchronously in the context of the actor,
  ;; while the caller waits synchronously for its result as transmitted via a completion.
  ;; After all actions with a given tx are run, the sync method must be called.
  ;; : (Fun 'a <- Key (Fun 'a <- State (Fun Unit <- State)) TX)
  action:
  (lambda (key f)
    (def c (make-completion))
    (def (k . res) (completion-post! c (list->values res)))
    (thread-send (<-key key) [Transform: f k])
    (completion-wait! c))

  ;; Asynchronously notify (1) the actor with the given key that (2) work with the current tx is done;
  ;; the actor will must synchronize with that tx being committed before it starts processing requests
  ;; for other txs.
  ;; : Unit <- Key TX
  sync-action:
  (lambda (key f)
    (defvalues (res tx)
      (action key (lambda (get-state set-state! tx) (values (f get-state set-state! tx) tx))))
    (sync-transaction tx)
    res)

  ;; State <- Key
  read:
  (lambda (key)
    ((thread-specific (<-key key)))))


;; TODO: handle mixin inheritance graph so we can make this a mixin rather than an alternative superclass
(.def (SavingDebug @ [] Key State sexp key-prefix)
  saving: =>
  (lambda (super)
    (fun (saving db-key state tx)
      (def key (<-bytes Key (subu8vector db-key (bytes-length key-prefix) (bytes-length db-key))))
      ;;(printf "SAVING ~s ~s => ~s\n" sexp (sexp<- Key key) (sexp<- State state))
      (super db-key state tx)))
  .resume: =>
  (lambda (super)
    (fun (resume key state tx)
      ;;(printf "RESUME ~s ~s => ~s\n" sexp (sexp<- Key key) (sexp<- State state))
      ;;(DBG resume-2:
      (super key state tx)
      )));;)

(.def (DebugPersistentActivity @ [SavingDebug PersistentActivity]))

(def (ensure-db-key key)
  (cond
   ((bytes? key) key)
   ((string? key) (string->bytes key))
   (else (error "Invalid db-key" key))))

(defstruct persistent-variable (mx type key value loaded?))
(def (get-persistent-variable pvar)
  (with-lock (persistent-variable-mx pvar)
    (lambda ()
      (unless (persistent-variable-loaded? pvar)
        (let (bytes (with-tx (tx) (db-get (persistent-variable-key pvar) tx)))
          (when bytes
            (let (val (<-bytes (persistent-variable-type pvar) bytes))
              (set! (persistent-variable-value pvar) val))))
        (set! (persistent-variable-loaded? pvar) #t))
      (persistent-variable-value pvar))))
(def (get-persistent-variable-set! pvar val)
  (with-lock (persistent-variable-mx pvar)
    (lambda ()
      (with-tx (tx) (db-put! (persistent-variable-key pvar) val tx))
      (set! (persistent-variable-loaded? pvar) #t)
      (set! (persistent-variable-value pvar) val))))
(def (%make-persistent-variable name type key initial-value)
  (make-persistent-variable (make-mutex 'name) type (ensure-db-key key) initial-value #f))
(defsyntax (define-persistent-variable stx)
  (syntax-case stx ()
    ((d name type key initial-value)
     (with-syntax ((setter (datum->syntax #'d (symbolify [#'name "-set!"]))))
       #'(begin
           (def pvar (%make-persistent-variable 'name type key initial-value))
           (defrule (name) (get-persistent-variable pvar))
           (defrule (setter val) (get-persistent-variable-set! pvar val)))))))
