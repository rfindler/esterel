#lang racket
(require "structs.rkt" "search-state.rkt"
         (for-syntax syntax/parse))

(provide
 (rename-out [-reaction reaction])
 (rename-out [-signal signal])
 par
 suspend
 with-trap
 (contract-out
  [react! (->* (reaction?)
               (#:emit (listof signal?))
               (hash/c signal? boolean? #:immutable #t #:flat? #t))]
  [in-reaction? (-> boolean?)]
  [signal-value (->* (signal?)
                     #:pre (in-reaction?)
                     boolean?)]
  [signal? (-> any/c boolean?)]
  [signal-name (-> signal? string?)]
  [emit (->* (signal?)
             #:pre (in-reaction?)
             void?)]
  [pause (->* () #:pre (in-reaction?) void?)]
  [exit-trap (-> trap? any)]
  [exn:fail:not-constructive? (-> any/c boolean?)]))


#|

When starting a reaction, we create a new thread that has
the state of all the emitted signals, the `instant loop` thread.
Then we also put the argument to `reaction` in its own thread,
and all of the operations (emit, pause, etc) all communicate
with the instant loop thread to determine the value of signals
and to set them.

Each time we do a `with-trap` we bind the variable to a new
trap value and store, in that value, a natural number. This number
is the depth of the trap that was just created and is stored
in a continuation mark (with-trap's body is not in tail position
wrt to the trap in order to avoid these marks colliding with each
other). We also stick an escape continuation into the trap
value. When exiting to a trap (that is not inside a nested par)
we just jump to the escape continuation in the trap.

Each `par` creates N different racket threads, one for each
argument of the par. These threads all have a continuation
mark set at the start with the trap counter at the par, which
is used to determine if a trap is going to cross the par when
it is exited. If a trap does have to exit the par, we communicate
with the instant thread to let it know that this arm of the par
has finished. When all arms of the par finish, then we decide
what to do with the par. This is handled by `maybe-finalize-a-par`
(which also uses `unpause-to-trap`).

To handle instantaneous reaction to absence, we simply block on
unemitted signals. When everything else has finished, there must
be one of those signals that is not going to be emitted (or else
the reaction isn't constructive) and so we just pick one to see
if it is. The picking is handled by search-state.rkt. We then run
on the assumption it is absent and we find out we were wrong if
it gets emitted. In order to prepare for being wrong, we grab
continuations of all the threads before running forward with the
assumption. If the assumption was wrong, we roll back and pick
a different signal to be absent. Eventually either the reaction
completes, or we run out of signals to pick. If the former, fine.
If the latter, we raise the non-constructive exception.

|#

(define-logger esterel)

(struct checkpoint-request (resp-chan))
(define reaction-prompt-tag (make-continuation-prompt-tag 'reaction))

(define current-signal-table (make-parameter #f))
(define (in-reaction?) (and (current-signal-table) #t))

(define-syntax (-signal stx)
  (syntax-case stx ()
    [(_) #`(mk-signal.1 '#,(syntax-local-name))]
    [(_ #:name n) #`(mk-signal.1 n)]
    [x (identifier? #'x) #`mk-signal.0]))

(define mk-signal.0
  (let ([signal
         (λ () (signal #f))])
    signal))

(define (mk-signal.1 x) (signal (format "~a" x)))

(define (emit a-signal)
  (define signal-table (current-signal-table))
  (channel-put (signal-table-emit-chan signal-table) a-signal))
  
(define (signal-value a-signal)
  (define signal-table (current-signal-table))
  (define resp-chan (make-channel))
  (channel-put (signal-table-signal-chan signal-table)
               (vector a-signal (current-thread) resp-chan))
  (let loop ([resp-chan resp-chan])
    (define maybe-val (channel-get resp-chan))
    (cond
      [(checkpoint-request? maybe-val)
       (loop
        (call/cc
         (λ (k)
           (channel-put (checkpoint-request-resp-chan maybe-val) k)
           resp-chan)
         reaction-prompt-tag))]
      [else maybe-val])))

(define-syntax (par stx)
  (syntax-case stx ()
    [(_ e ...)
     ;; do a fancy dance with the syntax here so that
     ;; the threads pick up the source location of the original `par` subexpressions
     (with-syntax ([(thunks ...)
                    (for/list ([e (in-list (syntax->list #'(e ...)))])
                      (quasisyntax/loc e (λ () #,e)))])
       #`(par/proc (list thunks ...)))]))

(define (par/proc thunks)
  (unless (in-reaction?) (error 'par "not in a reaction"))
  (define s (make-semaphore 0))
  (define before-par-trap-counter (get-current-trap-counter))
  (define result-chans+children-threads
    (for/set ([thunk (in-list thunks)])
      ;; par-child-result-chan : channel[(or/c #f trap? exn?)]
      (define par-child-result-chan (make-channel))
      (define par-child-thread
        (make-esterel-thread
         #:before-par-trap-counter before-par-trap-counter
         #:par-child-result-chan par-child-result-chan
         #:wait-on-sema s
         thunk))
      (cons par-child-result-chan par-child-thread)))
  (define signal-table (current-signal-table))
  (define checkpoint-or-par-result-chan (make-channel))
  (channel-put (signal-table-par-start-chan signal-table)
               (vector checkpoint-or-par-result-chan
                       (current-thread)
                       result-chans+children-threads))
  (for ([_ (in-list thunks)]) (semaphore-post s))
  (let loop ([pending-result-chans+par-threads result-chans+children-threads]
             [checkpoint-or-par-result-chan checkpoint-or-par-result-chan])
    (apply
     sync
     (handle-evt
      checkpoint-or-par-result-chan
      (λ (checkpoint-resp-chan-or-final-result)
        (match checkpoint-resp-chan-or-final-result
          [(? channel?)
           ;; here we have been asked to do a checkpoint
           (define-values (new-pending-result-chans+par-threads new-checkpoint-or-par-result-chan)
             (call/cc
              (λ (k)
                (channel-put checkpoint-resp-chan-or-final-result (vector before-par-trap-counter k))
                (values pending-result-chans+par-threads checkpoint-or-par-result-chan))
              reaction-prompt-tag))
           (when (set-empty? pending-result-chans+par-threads)
             (internal-error "asked for a par checkpoint with no children"))
           (loop new-pending-result-chans+par-threads new-checkpoint-or-par-result-chan)]
          [(? (or/c trap? exn?))
           (unless (set-empty? pending-result-chans+par-threads)
             (internal-error "exiting the par but still have children.1 ~s ~s ~s"
                             (current-thread)
                             checkpoint-resp-chan-or-final-result
                             pending-result-chans+par-threads))
           (exit-trap checkpoint-resp-chan-or-final-result)]
          [#f
           (unless (set-empty? pending-result-chans+par-threads)
             (internal-error "exiting the par but still have children.2 ~s ~s"
                             (current-thread) pending-result-chans+par-threads))
           (void)])))
     (for/list ([result-chan+pending-par-thread (in-set pending-result-chans+par-threads)])
       (match-define (cons result-chan pending-par-thread) result-chan+pending-par-thread)
       (handle-evt
        result-chan
        (λ (par-child-result)
          (channel-put (signal-table-par-partly-done-chan signal-table)
                       (vector pending-par-thread par-child-result))
          (loop (set-remove pending-result-chans+par-threads result-chan+pending-par-thread)
                checkpoint-or-par-result-chan)))))))

;; we didn't save the parameterization at the point of the pause/signal-value/par
;; and so we cannot restore it here; not sure if this is important or not, tho!
;; this function creates the par children thread when a par is first encountered
;; and it also creates the threads for when we fall back to a previous state
;; in the search for which signals to be absent. It doesn't create the main
;; reaction thread but probably things should be cleaned up a bit so it can
;; if before-par-trap-counter isn't #f, then we know we're creating a par child
;; thread and we set up that machinery; we don't need the marks it the thread is
;; the outermost thread because the code that looks up the marks knows what to
;; do when it does not find the mark there (this might also be something to clean
;; up and make more uniform)
(define (make-esterel-thread #:before-par-trap-counter before-par-trap-counter
                             #:par-child-result-chan par-child-result-chan
                             #:wait-on-sema [s #f]
                             thunk)
  (define thunk-to-run
    (cond
      [before-par-trap-counter
       (λ ()
         (when s (semaphore-wait s))
         (channel-put
          par-child-result-chan
          (let/ec escape
            (define original-uncaught-exception-handler
              (uncaught-exception-handler))
            (parameterize ([uncaught-exception-handler
                            (λ (x)
                              (if (kernel-esterel.rkt::internal-error? x)
                                  (original-uncaught-exception-handler x)
                                  ;; what about breaks?
                                  (escape
                                   (raise-argument->exn x))))])
              (with-continuation-mark trap-start-of-par-mark
                (vector escape before-par-trap-counter)
                (with-continuation-mark trap-counter-mark before-par-trap-counter
                  (begin
                    (call-with-continuation-prompt
                     thunk
                     reaction-prompt-tag)
                    #f)))))))]
      [else
       (λ ()
         (call-with-continuation-prompt
          thunk
          reaction-prompt-tag))]))
  ;; this renaming doesn't work when rebuilding the threads currently
  (thread (procedure-rename thunk-to-run (object-name thunk))))

(define (raise-argument->exn x)
  (if (exn? x)
      x
      (make-exn:fail (format "uncaught exn: ~e" x)
                     (current-continuation-marks))))

;; outermost-trap : (or/c #f trap? exn?) (or/c #f trap? exn?) -> (or/c #f trap? exn?)
(define (outermost-trap t1 t2)
  (cond
    [(or (exn? t1) (exn? t2))
     (if (exn? t1) t1 t2)]
    [(and (trap? t1) (trap? t2))
     (if (< (trap-counter t1) (trap-counter t2))
         t1
         t2)]
    [else (or t1 t2)]))

(define (pause)
  (define signal-table (current-signal-table))
  (define resp-chan (make-channel))
  (channel-put (signal-table-pause-chan signal-table)
               (vector (current-thread) resp-chan))
  (let loop ([resp-chan resp-chan])
    (define val (channel-get resp-chan))
    (cond
      [(exn? val) (raise val)]
      [(trap? val) (exit-trap val)]
      [(channel? val)
       (loop
        (call/cc
         (λ (k)
           (channel-put val k)
           resp-chan)
         reaction-prompt-tag))]
      [else
       (define iter
         (continuation-mark-set->iterator
          (current-continuation-marks)
          (list suspend-mark)))
       (let loop ([iter iter])
         (define-values (next-val next-iter) (iter))
         (when next-val
           (when ((vector-ref next-val 0)) (pause))
           (loop next-iter)))
       val])))

(define-syntax (suspend stx)
  (syntax-case stx ()
    [(_ e-body e-when)
     #'(suspend/proc (λ () e-body) (λ () e-when))]))

(define suspend-mark (gensym 'suspend))
(define (suspend/proc body signal-thunk)
  (unless (in-reaction?) (error 'suspend "not in a reaction"))
  (with-continuation-mark suspend-mark signal-thunk
    ;; we don't want the body to be in tail position
    (begin0
      (body)
      (void))))

(define-syntax (with-trap stx)
  (syntax-parse stx
    [(_ t:identifier body1 body ...)
     #'(let/ec escape
         (let ([t (build-trap 't escape)])
           (with-trap/proc t (λ () body1 body ...))))]))

(define (build-trap name escape)
  (trap name (get-current-trap-counter) escape))

(define (get-current-trap-counter) (or (continuation-mark-set-first #f trap-counter-mark) 0))
(define trap-counter-mark (gensym 'trap-counter))
(define trap-start-of-par-mark (gensym 'trap-counter))

(define (with-trap/proc trap body)
  (define counter (trap-counter trap))
  (with-continuation-mark trap-counter-mark (+ counter 1)
    ;; we don't need to add something to block
    ;; multiple uses of the mark because the let/ec
    ;; introduces a non-tail context (for repeated
    ;; `with-mark`s).
    (body)))

;; internally, this might be called with an exception, but from the
;; outside, it is called only with traps (thanks to the contract) and
;; exceptions have to be `raise`d.
(define (exit-trap exn-or-trap)
  (define start-of-par-counter (continuation-mark-set-first #f trap-start-of-par-mark))
  (cond
    [(or (not start-of-par-counter)
         ;; the start of the par value will be the same as
         ;; the counter used for the first trap inside the par
         ;; so this should be an inclusive comparison
         (exn? exn-or-trap)
         (<= (vector-ref start-of-par-counter 1) (trap-counter exn-or-trap)))
     ;; here the trap doesn't span a par, so we can just escape
     (if (exn? exn-or-trap)
         (raise exn-or-trap)
         ((trap-escape exn-or-trap) (void)))]
    [else
     ;; here we tell the enclosing par that a trap has happened
     ((vector-ref start-of-par-counter 0) exn-or-trap)]))

(struct signal-table (signal-chan
                      emit-chan
                      par-start-chan par-partly-done-chan
                      pause-chan instant-chan
                      react-thread-done-chan))
  
(struct reaction (signal-table) #:mutable)
(define-syntax-rule (-reaction e1 e2 ...) (reaction/proc (λ () e1 e2 ...)))
(define (reaction/proc thunk)
  (define the-signal-table
    (signal-table (make-channel) (make-channel) (make-channel)
                  (make-channel) (make-channel) (make-channel) (make-channel)))
  (thread (λ () (run-reaction-thread thunk the-signal-table)))
  (reaction the-signal-table))

(define (react! a-reaction #:emit [signals-to-emit '()])
  (define signal-table (reaction-signal-table a-reaction))
  (define maybe-signals
    (cond
      [(equal? signal-table 'non-constructive)
       'non-constructive]
      [(exn? signal-table)
       signal-table]
      [else
       (define instant-complete-chan (make-channel))
       (channel-put (signal-table-instant-chan signal-table)
                    (cons signals-to-emit instant-complete-chan))
       (define maybe-signals (channel-get instant-complete-chan))
       (when (or (equal? maybe-signals 'non-constructive)
                 (exn? maybe-signals))
         (set-reaction-signal-table! a-reaction maybe-signals))
       maybe-signals]))
  (match maybe-signals
    ['non-constructive
     (raise
      (exn:fail:not-constructive
       "react!: the program is not constructive"
       (current-continuation-marks)))]
    [(? exn?) (raise maybe-signals)]
    [#f (error 'react! "a reaction is already running")]
    [else maybe-signals]))

(struct exn:fail:not-constructive exn:fail ())

(define (run-reaction-thread thunk the-signal-table)
  (define first-instant-sema (make-semaphore 0))
  (define reaction-thread
    (let ([first-instant-sema first-instant-sema])
      (parameterize ([current-signal-table the-signal-table])
        (define (reaction-thread-thunk)
          (semaphore-wait first-instant-sema)
          (call-with-continuation-prompt
           (λ ()
             (define react-thread-done-chan
               (signal-table-react-thread-done-chan the-signal-table))
             (let/ec escape
               (call-with-exception-handler
                (λ (exn)
                  (cond
                    [(kernel-esterel.rkt::internal-error? exn)
                     exn]
                    [else
                     (channel-put react-thread-done-chan (raise-argument->exn exn))
                     (escape (void))]))
                thunk)
               (channel-put react-thread-done-chan #f)))
           reaction-prompt-tag))
        (thread reaction-thread-thunk))))
  (match-define (signal-table signal-chan emit-chan
                              par-start-chan par-partly-done-chan
                              pause-chan instant-chan
                              react-thread-done-chan)
    the-signal-table)


;                                                                                    
;                                                                                    
;                                                                                    
;                                                                                    
;  ;;;                  ;                    ;               ;            ;          
;                     ;;;                  ;;;             ;;;          ;;;          
;  ;;; ;;; ;;   ;;;;  ;;;;  ;;;;;  ;;; ;;  ;;;;      ;;;;  ;;;;  ;;;;;  ;;;;   ;;;;  
;  ;;; ;;;;;;; ;;; ;; ;;;; ;;;;;;; ;;;;;;; ;;;;     ;;; ;; ;;;; ;;;;;;; ;;;;  ;; ;;; 
;  ;;; ;;; ;;; ;;;    ;;;  ;;  ;;; ;;; ;;; ;;;      ;;;    ;;;  ;;  ;;; ;;;  ;;; ;;; 
;  ;;; ;;; ;;;  ;;;;  ;;;    ;;;;; ;;; ;;; ;;;       ;;;;  ;;;    ;;;;; ;;;  ;;;;;;; 
;  ;;; ;;; ;;;    ;;; ;;;  ;;; ;;; ;;; ;;; ;;;         ;;; ;;;  ;;; ;;; ;;;  ;;;     
;  ;;; ;;; ;;; ;; ;;; ;;;; ;;; ;;; ;;; ;;; ;;;;     ;; ;;; ;;;; ;;; ;;; ;;;;  ;;;;;; 
;  ;;; ;;; ;;;  ;;;;   ;;;  ;;;;;; ;;; ;;;  ;;;      ;;;;   ;;;  ;;;;;;  ;;;   ;;;;  
;                                                                                    
;                                                                                    
;                                                                                    
;                                                                                    

  
  (define non-constructive-program? #f)

  ;; if a signal isn't mapped, its value isn't yet known
  (define/contract signals
    (hash/c signal? boolean? #:flat? #t #:immutable #t)
    (hash))

  ;; the latest-exn is set to an exception when we just
  ;; finished a reaction; this might indicate a bad guess
  ;; so we'll go back and make another guess (until we
  ;; exhaust all guesses) if this is not #f
  (define/contract latest-exn
    (or/c #f exn?)
    #f)

  ;; this is all of the exceptions that were raised
  ;; during the guessing when running the current instant
  (define/contract raised-exns
    (set/c exn?)
    (set))

  (struct blocked-thread (thread resp-chan) #:transparent)
    
  ;; threads that are blocked, waiting for a signal's value to be decided
  ;; (a missing entry is the same as the empty list)
  (define/contract signal-waiters
    (hash/c signal? (listof blocked-thread?) #:flat? #t #:immutable #t)
    (hash))
  (define (add-signal-waiter! a-signal the-thread resp-chan)
    (define a-blocked-thread (blocked-thread the-thread resp-chan))
    (define new-waiters (cons a-blocked-thread (hash-ref signal-waiters a-signal '())))
    (set! signal-waiters (hash-set signal-waiters a-signal new-waiters)))

  ;; find-signal : thread -> (cons signal blocked-thread) or #f
  ;; returns the signal that `thread` is blocked on, or #f it is isn't blocked on a signal
  (define (find-signal thread)
    (for*/or ([(signal blocked-threads) (in-hash signal-waiters)]
              [a-blocked-thread (in-list blocked-threads)])
      (and (equal? (blocked-thread-thread a-blocked-thread) thread)
           (cons signal a-blocked-thread))))

  ;; each paused thread is blocked on the corresponding channel
  ;; when a par's threads are all paused, the parent thread does
  ;; *not* end up here (even though it is morally paused)
  (define/contract paused-threads
    (hash/c thread? channel? #:flat? #t #:immutable #t)
    (hash))

  ;; (set/c hash)
  ;; all of the threads in the reaction that are:
  ;;    - not blocked on a signal
  ;;    - not paused
  ;;    - not a par-parent thread
  (define running-threads (set reaction-thread))
  (define/contract (add-running-thread t)
    (-> thread? void?)
    (when (set-member? running-threads t)
      (internal-error "adding a running thread but it was already running ~s" t))
    (when (hash-has-key? parent->par-state t)
      (internal-error "adding a running thread but it was a par parent ~s" t))
    (set! running-threads (set-add running-threads t)))
  (define/contract (add-running-threads ts)
    (-> (set/c thread?) void?)
    (for ([t (in-set ts)])
      (when (set-member? running-threads t)
        (internal-error "adding a running thread (via add-running-threads) but it was already running ~s" t))
      (when (hash-has-key? parent->par-state t)
        (internal-error "adding a running thread (via add-running-threads) but it was a par parent ~s" t)))
    (set! running-threads (set-union running-threads ts)))
  (define (remove-running-thread t)
    (unless (set-member? running-threads t)
      (internal-error "removing a running thread but it was not running ~s" t))
    (set! running-threads (set-remove running-threads t)))

  ;; a child thread (of a par) points to its parent
  (define/contract par-parents
    (hash/c thread? thread? #:flat? #t #:immutable #t)
    (hash))

  ;; this tracks the state of the par;
  ;; - each parent thread listens for two different messages on the
  ;;   result/checkpoint-chan, either requests to grab the continuation
  ;;   or requests for the checkpoint
  ;; - signal-waiting is the set of children threads that are blocked waiting
  ;;   on a signal's value
  ;; - paused is the set of paused children threads
  ;; - active is the set of threads that are running
  ;; - trap is the highest trap (or exn) that any of these children have
  ;;   exited to, or #f if none of them have exited to a trap
  (struct par-state (result/checkpoint-chan signal-waiting paused active trap) #:transparent)
  (define/contract parent->par-state
    (hash/c thread? (struct/c par-state channel? (set/c thread?) (set/c thread?) (set/c thread?) (or/c #f trap? exn?))
            #:flat? #t #:immutable #t)
    (hash))

  ;; (or/c #f (chan/c (or/c (hash/c signal? boolean?) #f)))
  ;; #f means we're not in an instant, chan means we are.
  ;; #f is sent back is a message to signal an error
  ;; in `react!` and the hash are the signal values that
  ;; got taken in this instant. Any chan that gets put into
  ;; never gets, #f, tho. Only those that don't get put here do
  (define instant-complete-chan #f)

  (define (log-par-state)
    (log-esterel-debug
     "running ~a\n         ~a"
     (if (set-empty? running-threads)
         "... none"
         (for/fold ([s ""])
                   ([thd (in-set running-threads)]
                    [i (in-naturals)])
           (string-append s (if (= i 0) "" " ") (format "~s" thd))))
     (cond
       [(zero? (hash-count parent->par-state)) "no pars running"]
       [else
        (define sp (open-output-string))
        (define nl "\n         ")
        (for ([(thread a-par-state) (in-hash parent->par-state)]
              [i (in-naturals)])
          (unless (zero? i) (display nl sp))
          (match-define (par-state result/checkpoint-chan signal-waiting paused active a-trap) a-par-state)
          (fprintf sp "par parent: ~s; trap: ~a" thread a-trap)
          (display nl sp)
          (fprintf sp "  sig: ~s" signal-waiting)
          (display nl sp)
          (fprintf sp "  pau: ~s" paused)
          (display nl sp)
          (fprintf sp "  act: ~s" active))
        (get-output-string sp)])))



;                                                           
;                                                           
;                                                           
;                                                           
;               ;;; ;;;     ;;;                     ;;;     
;               ;;; ;;;     ;;;                     ;;;     
;  ;;; ;; ;;;   ;;; ;;;     ;;; ;;   ;;;;;    ;;;   ;;;  ;;;
;  ;;;;; ;;;;;  ;;; ;;;     ;;;;;;; ;;;;;;;  ;;;;;  ;;; ;;; 
;  ;;;  ;;; ;;; ;;; ;;;     ;;; ;;; ;;  ;;; ;;;  ;; ;;;;;;  
;  ;;;  ;;; ;;; ;;; ;;;     ;;; ;;;   ;;;;; ;;;     ;;;;;;  
;  ;;;  ;;; ;;; ;;; ;;;     ;;; ;;; ;;; ;;; ;;;  ;; ;;;;;;; 
;  ;;;   ;;;;;  ;;; ;;;     ;;;;;;; ;;; ;;;  ;;;;;  ;;; ;;; 
;  ;;;    ;;;   ;;; ;;;     ;;; ;;   ;;;;;;   ;;;   ;;;  ;;;
;                                                           
;                                                           
;                                                           
;                                                           

  
  (define se-st (new-search-state))

  ;; this gets set to #t when we discover that signal
  ;; had a wrong guess (we guessed it won't be emitted but
  ;; it actually gets emitted)
  (define wrong-guess? #f)
  
  (define (choose-a-signal-to-be-absent)
    (define signal-to-be-absent
      (cond
        [(or latest-exn wrong-guess?)
         (when latest-exn
           (set! raised-exns (set-add raised-exns latest-exn)))
         (set! wrong-guess? #f)
         (set! latest-exn #f)
         (define-values (rollback-point choice) (fail! se-st))
         (cond
           [choice
            (rollback! rollback-point)
            choice]
           [else
            ;; after this function returns, we always go back to the
            ;; main loop in the handler thread; with this flag set
            ;; it always just disables everything
            (set! non-constructive-program? #t)])]
        [else
         (continue! se-st
                    (current-rollback-point)
                    (hash-keys signals)
                    (hash-keys signal-waiters))]))
    (unless non-constructive-program?
      (log-esterel-debug "~a: chose ~a to be absent" (eq-hash-code (current-thread)) signal-to-be-absent)
      (log-par-state)
      (define blocked-threads (hash-ref signal-waiters signal-to-be-absent))
      (set! signals (hash-set signals signal-to-be-absent #f))
      (set! signal-waiters (hash-remove signal-waiters signal-to-be-absent))
      (for ([a-blocked-thread (in-list blocked-threads)])
        (match-define (blocked-thread thread resp-chan) a-blocked-thread)
        (channel-put resp-chan #f)
        (add-running-thread thread)
        (define parent-thread (hash-ref par-parents thread #f))
        (when parent-thread
          (define old-par-state (hash-ref parent->par-state parent-thread))
          (define new-par-state
            (struct-copy par-state old-par-state
                         [active (set-add (par-state-active old-par-state) thread)]
                         [signal-waiting (set-remove (par-state-signal-waiting old-par-state) thread)]))
          (set! parent->par-state (hash-set parent->par-state parent-thread new-par-state))))))

  ;; rb-tree : rb-tree?
  ;; signals : (hash/c signal? boolean? #:flat? #t #:immutable #t)
  (struct rollback-point (rb-tree signals))
  (define (rollback! a-rollback-point)
    (match-define (rollback-point rb-tree rb-signals) a-rollback-point)
    (set! signals rb-signals)
    (set! latest-exn #f)
    (set! signal-waiters (hash))
    (set! paused-threads (hash))
    (set! par-parents (hash))
    (set! parent->par-state (hash))
    (set! reaction-thread (rebuild-threads-from-rb-tree rb-tree)))
  (define (current-rollback-point) (rollback-point (build-rb-tree-from-current-state) signals))

  (struct rb-tree ())

  ;; cont : continuation[listof thread]
  ;; signal-waiting : (set/c rb-tree?)
  ;; paused : (set/c rb-tree?)
  ;; trap : (or/c trap? exn? #f)
  ;; before-par-trap-counter : natural?
  (struct rb-par rb-tree (cont signal-waiting paused trap before-par-trap-counter))

  ;; cont : continuation[channel]
  (struct rb-paused rb-tree (cont))

  ;; signal : signal?
  ;; cont : continuation[channel]
  (struct rb-blocked rb-tree (signal cont))

  ;; build-rb-tree-from-current-state : -> rb-tree?
  (define (build-rb-tree-from-current-state)
    (define k-chan (make-channel))
    (define a-checkpoint-request (checkpoint-request k-chan))
    (let loop ([thread reaction-thread])
      (cond
        [(hash-has-key? parent->par-state thread)
         (match-define (par-state checkpoint/result-chan signal-waiting paused active a-trap) (hash-ref parent->par-state thread))
         (channel-put checkpoint/result-chan k-chan)
         (match-define (vector before-par-trap-counter par-continuation) (channel-get k-chan))
         (rb-par
          par-continuation
          (for/set ([child (in-set signal-waiting)])
            (loop child))
          (for/set ([child (in-set paused)])
            (loop child))
          a-trap
          before-par-trap-counter)]
        [(hash-has-key? paused-threads thread)
         (define paused-chan (hash-ref paused-threads thread))
         (channel-put paused-chan k-chan)
         (rb-paused (channel-get k-chan))]
        [(find-signal thread)
         =>
         (λ (signal+blocked-thread)
           (match-define (cons signal (blocked-thread thread resp-chan)) signal+blocked-thread)
           (channel-put resp-chan a-checkpoint-request)
           (rb-blocked signal (channel-get k-chan)))]
        [else (internal-error "lost a thread ~s" thread)])))

  ;; rebuild-threads-from-rb-tree : rb-tree? -> thread
  ;; returns the new reaction thread
  (define (rebuild-threads-from-rb-tree rb-tree)
    (let loop ([rb-tree rb-tree]
               [par-child-result-chan #f]
               [parent-before-par-trap-counter #f])
      (match rb-tree
        [(rb-par par-cont rb-signal-waiting rb-paused a-trap before-par-trap-counter)
         ;; we have to do this strange dance with `sema` in order to make
         ;; the recursive call to `loop`, as we need the par parent
         ;; thread to make the call. So we avoid race
         ;; conditions by blocking on `sema` until we've completed the
         ;; recursive calls and updated the instant state
         (define sema (make-semaphore))
         (define par-parent-thread
           (parameterize ([current-signal-table the-signal-table])
             (make-esterel-thread
              #:before-par-trap-counter parent-before-par-trap-counter
              #:par-child-result-chan par-child-result-chan
              (λ ()
                (define (get-result-chans+par-children rb-children)
                  (for/set ([child (in-set rb-children)])
                    (define par-child-result-chan (make-channel))
                    (cons par-child-result-chan
                          (loop child
                                par-child-result-chan
                                before-par-trap-counter))))
                (define (drop-result-chans result-chans+children-threads)
                  (for/set ([result-chan+children-thread (in-set result-chans+children-threads)])
                    (cdr result-chan+children-thread)))

                (define parent-thread (current-thread))
                (define checkpoint/result-chan (make-channel))
                (define this-par-result-chans+signal-waiting-children (get-result-chans+par-children rb-signal-waiting))
                (define this-par-result-chans+paused-children (get-result-chans+par-children rb-paused))
                (define signal-waiting (drop-result-chans this-par-result-chans+signal-waiting-children))
                (define paused (drop-result-chans this-par-result-chans+paused-children))
                (for ([child-thread (in-set (set-union signal-waiting paused))])
                  (set! par-parents (hash-set par-parents child-thread parent-thread)))
                (set! parent->par-state
                      (hash-set parent->par-state
                                parent-thread
                                (par-state checkpoint/result-chan
                                           signal-waiting
                                           paused
                                           (set)
                                           a-trap)))
                (semaphore-post sema)
                (par-cont (set-union this-par-result-chans+signal-waiting-children
                                     this-par-result-chans+paused-children)
                          checkpoint/result-chan)))))
         (semaphore-wait sema)
         par-parent-thread]
        [(rb-paused cont)
         (define resp-chan (make-channel))
         (define paused-thread
           (parameterize ([current-signal-table the-signal-table])
             (make-esterel-thread
              #:before-par-trap-counter parent-before-par-trap-counter
              #:par-child-result-chan par-child-result-chan
              (λ () (cont resp-chan)))))
         (set! paused-threads (hash-set paused-threads paused-thread resp-chan))
         paused-thread]
        [(rb-blocked a-signal cont)
         (define resp-chan (make-channel))
         (define blocked-thread
           (parameterize ([current-signal-table the-signal-table])
             (make-esterel-thread
              #:before-par-trap-counter parent-before-par-trap-counter
              #:par-child-result-chan par-child-result-chan
              (λ () (cont resp-chan)))))
         (add-signal-waiter! a-signal blocked-thread resp-chan)
         blocked-thread])))
  
;                                                                               
;                                                                               
;                                                                               
;                                                                               
;  ;;;                  ;                    ;      ;;;                         
;                     ;;;                  ;;;      ;;;                         
;  ;;; ;;; ;;   ;;;;  ;;;;  ;;;;;  ;;; ;;  ;;;;     ;;;   ;;;     ;;;   ;;; ;;  
;  ;;; ;;;;;;; ;;; ;; ;;;; ;;;;;;; ;;;;;;; ;;;;     ;;;  ;;;;;   ;;;;;  ;;;;;;; 
;  ;;; ;;; ;;; ;;;    ;;;  ;;  ;;; ;;; ;;; ;;;      ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;;  ;;;;  ;;;    ;;;;; ;;; ;;; ;;;      ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;;    ;;; ;;;  ;;; ;;; ;;; ;;; ;;;      ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;; ;;; ;;;; ;;; ;;; ;;; ;;; ;;;;     ;;;  ;;;;;   ;;;;;  ;;;;;;; 
;  ;;; ;;; ;;;  ;;;;   ;;;  ;;;;;; ;;; ;;;  ;;;     ;;;   ;;;     ;;;   ;;; ;;  
;                                                                       ;;;     
;                                                                       ;;;     
;                                                                               
;                                                                               

  (define (maybe-finalize-a-par parent-thread)
    (match-define (par-state result/checkpoint-chan signal-waiting paused active a-trap)
      (hash-ref parent->par-state parent-thread))

    (when (and (set-empty? signal-waiting)
               (set-empty? active))
      ;; here we know that none of the threads in the par are going to do
      ;; anything more in this instant. time to close the par up
      (cond
        [(set-empty? paused)
         ;; if there are no paused threads, then we know that the result
         ;; of this par is the trap (which might be #f meaning no actual
         ;; trap); send it to the parent thread
         (channel-put result/checkpoint-chan a-trap)
         (set! parent->par-state (hash-remove parent->par-state parent-thread))
         (add-running-thread parent-thread)]
        [a-trap
         ;; here we have a trap (or an exn) and there is at least one paused
         ;; thread; we need to tell all the paused threads to exit to the trap
         ;; when that finishes we'll be back here to close up the par itself
         (unpause-to-trap parent-thread a-trap)]
        [else
         ;; here we should count the entire par as paused, since there are no
         ;; traps and at least one paused thread. update the state and recur
         ;; with the parent thread
         (define parent-parent-thread (hash-ref par-parents parent-thread #f))
         (when parent-parent-thread
           (define old-par-state (hash-ref parent->par-state parent-parent-thread))
           (define new-par-state
             (struct-copy
              par-state old-par-state
              [active (set-remove (par-state-active old-par-state) parent-thread)]
              [paused (set-add (par-state-paused old-par-state) parent-thread)]))
           (set! parent->par-state (hash-set parent->par-state
                                        parent-parent-thread
                                        new-par-state))
           (maybe-finalize-a-par parent-parent-thread))])))

  ;; unpause-to-trap : thread? (or/c trap? exn?) -> void
  ;; unpause all of the paused children threads of `parent-thread`, sending them to `trap-to-exit-to` instead
  (define (unpause-to-trap parent-thread trap-to-exit-to)
    (let loop ([parent-thread parent-thread]
               [first-one? #t])
      (match-define (par-state result/checkpoint-chan signal-waiting paused active the-trap-of-this-par)
        (hash-ref parent->par-state parent-thread))
      (unless first-one?
        (when the-trap-of-this-par
          (internal-error "found a par as we went down to unpause things that has a trap, par-parent: ~s"
                          parent-thread)))
      (for ([paused-thread (in-set paused)])
        (define resp-chan (hash-ref paused-threads paused-thread #f))
        (cond
          [resp-chan
           ;; this is a "leaf" thread that called `pause`, wake it up with the trap
           (channel-put resp-chan trap-to-exit-to)
           (add-running-thread paused-thread)
           (set! paused-threads (hash-remove paused-threads paused-thread))]
          [else
           ;; here we have a parent thread, recur down
           (loop paused-thread #f)]))
      (set! parent->par-state
            (hash-set parent->par-state
                      parent-thread
                      (par-state result/checkpoint-chan signal-waiting (set)
                                 (set-union paused active)
                                 the-trap-of-this-par)))))
  
  (let loop ()

    ;; this is a kind of abuse of the logging system; we skip these checks unless
    ;; someone is listening on the debugging logger
    (log-esterel-debug
     "checking invariants of par-parents / running-threads / parent->par-state~a"
     (begin
       (for ([(par-parent a-par-state) (in-hash parent->par-state)])
         (match-define (par-state checkpoint/result-chan signal-waiting paused active a-trap) a-par-state)
         (for ([child-thread (in-set (set-union signal-waiting paused active))])
           (unless (equal? (hash-ref par-parents child-thread #f) par-parent)
             (internal-error "parents relationship lost; parent ~s child ~s" par-parent child-thread)))

         (for ([active-child-thread (in-set active)])
           (unless (or (set-member? running-threads active-child-thread)
                       (hash-has-key? parent->par-state active-child-thread))
             (internal-error "par ~s has active child ~s that isn't in a parent nor in running-threads"
                             par-parent active-child-thread))))
       ""))

    (cond
      [non-constructive-program?
       (log-esterel-debug "~a: non constructive program ~s" (eq-hash-code (current-thread)) raised-exns)
       (log-par-state)
       (channel-put instant-complete-chan
                    (if (set-empty? raised-exns)
                        'non-constructive
                        (set-first raised-exns)))
       ;; when we send back 'non-constructive or an exception, then we will
       ;; never come back to this thread again, so just let it expire
       (void)]

      ;; the instant is over
      [(and (= 0 (hash-count signal-waiters))
            (set-empty? running-threads)
            instant-complete-chan)
       (log-esterel-debug "~a: instant is over: ~s" (eq-hash-code (current-thread)) (or latest-exn wrong-guess?))
       (log-par-state)
       (cond
         [(or latest-exn wrong-guess?)
          ;; although we got to the end of the instant, we did so with
          ;; a wrong guess for signal absence; go back and try again
          (choose-a-signal-to-be-absent)
          (loop)]
         [else
          ;; close the instant down and let `react!` know
          (channel-put instant-complete-chan signals)
          (set! instant-complete-chan #f)
          (set! signals (hash)) ;; reset the signals in preparation for the next instant
          (set! raised-exns (set)) ;; reset the exns (as they are now defunct; bad guesses)
          (loop)])]

      ;; an instant is not runnning, wait for one to start (but don't wait for other stuff)
      [(not instant-complete-chan)
       (log-esterel-debug "~a: waiting for an instant to start" (eq-hash-code (current-thread)))
       (log-par-state)
       (sync
        (handle-evt
         instant-chan
         (λ (signals-to-emit+instant-complete-chan)
           (match-define (cons signals-to-emit _instant-complete-chan)
             signals-to-emit+instant-complete-chan)
           (set! signals (for/fold ([signals signals])
                                   ([signal-to-emit (in-list signals-to-emit)])
                           (hash-set signals signal-to-emit #t)))
           (when first-instant-sema
             ;; this blocks starting the first instant; after that it isn't needed
             (semaphore-post first-instant-sema)
             (set! first-instant-sema #f))
           (set! instant-complete-chan _instant-complete-chan)
           (for ([(paused-thread resp-chan) (in-hash paused-threads)])
             (channel-put resp-chan (void))
             (add-running-thread paused-thread))
           (set! paused-threads (hash))
           (set! parent->par-state
                 (for/hash ([(parent-thread a-par-state) (in-hash parent->par-state)])
                   (match-define (par-state checkpoint/result-chan signal-waiting paused active a-trap) a-par-state)
                   ;; paused threads become active threads and there are no more paused threads when
                   ;; picking up after an instant has terminated
                   (values parent-thread (par-state checkpoint/result-chan signal-waiting (set) paused #f))))
           (loop))))]

      ;; nothing is running, but at least one thread is waiting for a signal's value
      [(set-empty? running-threads)
       (when (= 0 (hash-count signal-waiters))
         (internal-error "expected someone to be blocked on a signal"))
       (choose-a-signal-to-be-absent)
       (loop)]

      ;; an instant is running, handle the various things that can happen during it
      [else
       (sync
        (handle-evt
         signal-chan
         (λ (s+resp)
           (match-define (vector a-signal the-thread resp-chan) s+resp)
           (log-esterel-debug "~a: waiting on a signal ~s ~s ~s"
                              (eq-hash-code (current-thread))
                              a-signal
                              (hash-ref signals a-signal 'unknown)
                              the-thread)
           (log-par-state)
           (match (hash-ref signals a-signal 'unknown)
             ['unknown
              (remove-running-thread the-thread)
              (add-signal-waiter! a-signal the-thread resp-chan)
              (define parent-thread (hash-ref par-parents the-thread #f))
              (when parent-thread
                (define old-par-state (hash-ref parent->par-state parent-thread))
                (define new-par-state
                  (struct-copy
                   par-state old-par-state
                   [active (set-remove (par-state-active old-par-state) the-thread)]
                   [signal-waiting (set-add (par-state-signal-waiting old-par-state) the-thread)]))
                (set! parent->par-state (hash-set parent->par-state parent-thread new-par-state)))]
             [#f
              (channel-put resp-chan #f)]
             [#t
              (channel-put resp-chan #t)])
           (loop)))
        (handle-evt
         emit-chan
         (λ (a-signal)
           (log-esterel-debug "~a: emitting ~s ~s"
                              (eq-hash-code (current-thread))
                              a-signal
                              (hash-ref signals a-signal 'unknown))
           (log-par-state)
           (match (hash-ref signals a-signal 'unknown)
             ['unknown
              (set! signals (hash-set signals a-signal #t))
              (for ([a-blocked-thread (hash-ref signal-waiters a-signal '())])
                (match-define (blocked-thread thread resp-chan) a-blocked-thread)
                (channel-put resp-chan #t)
                (define parent-thread (hash-ref par-parents thread #f))
                (when parent-thread
                  (define old-par-state (hash-ref parent->par-state parent-thread))
                  (define new-par-state
                    (struct-copy
                     par-state old-par-state
                     [signal-waiting (set-remove (par-state-signal-waiting old-par-state) thread)]
                     [active (set-add (par-state-active old-par-state) thread)]))
                  (set! parent->par-state (hash-set parent->par-state parent-thread new-par-state)))
                (add-running-thread thread))
              (set! signal-waiters (hash-remove signal-waiters a-signal))]
             [#t
              (void)]
             [#f
              (set! wrong-guess? #t)])
           (loop)))
        (handle-evt
         par-start-chan
         (λ (checkpoint/result-chan+parent+children-threads)
           (match-define (vector checkpoint/result-chan parent-thread result-chans+children-threads)
             checkpoint/result-chan+parent+children-threads)
           (define children-threads (for/set ([x (in-set result-chans+children-threads)]) (cdr x)))
           (log-esterel-debug "~a: starting a par ~s ~s"
                              (eq-hash-code (current-thread))
                              parent-thread children-threads)
           (log-par-state)
           (add-running-threads children-threads)
           (remove-running-thread parent-thread)
           (set! parent->par-state (hash-set parent->par-state parent-thread (par-state checkpoint/result-chan (set) (set) children-threads #f)))
           (for ([child-thread (in-set children-threads)])
             (set! par-parents (hash-set par-parents child-thread parent-thread)))
           (loop)))
        (handle-evt
         par-partly-done-chan
         (λ (done-thread+trap)
           (match-define (vector done-thread new-trap) done-thread+trap)
           (define parent-thread (hash-ref par-parents done-thread))
           (log-esterel-debug "~a: one thread in a par finished ~s trap ~s"
                              (eq-hash-code (current-thread))
                              done-thread
                              new-trap)
           (log-par-state)
           (remove-running-thread done-thread)

           (define old-par-state (hash-ref parent->par-state parent-thread))
           (define new-par-state
             (struct-copy par-state old-par-state
                          [active (set-remove (par-state-active old-par-state) done-thread)]
                          [trap (outermost-trap (par-state-trap old-par-state) new-trap)]))
           (set! parent->par-state (hash-set parent->par-state parent-thread new-par-state))
           (set! par-parents (hash-remove par-parents done-thread))
           (maybe-finalize-a-par parent-thread)
           (loop)))
        (handle-evt
         pause-chan
         (λ (thread+resp-chan)
           (match-define (vector paused-thread resp-chan) thread+resp-chan)
           (log-esterel-debug "~a: paused ~s" (eq-hash-code (current-thread)) paused-thread)
           (log-par-state)
           (set! paused-threads (hash-set paused-threads paused-thread resp-chan))
           (remove-running-thread paused-thread)
           (define parent-thread (hash-ref par-parents paused-thread #f))
           (when parent-thread
             (define old-par-state (hash-ref parent->par-state parent-thread))
             (define new-par-state
               (struct-copy par-state old-par-state
                            [paused (set-add (par-state-paused old-par-state) paused-thread)]
                            [active (set-remove (par-state-active old-par-state) paused-thread)]))
             (set! parent->par-state (hash-set parent->par-state parent-thread new-par-state))
             (maybe-finalize-a-par parent-thread))
           (loop)))
        (handle-evt
         instant-chan
         (λ (signals-to-emit+instant-complete-chan)
           (match-define (cons signals-to-emit _instant-complete-chan)
             signals-to-emit+instant-complete-chan)
           ;; someone tried to start an instant while one is running
           ;; let them know there's an error
           (channel-put _instant-complete-chan #f)
           (loop)))
        (handle-evt
         react-thread-done-chan
         (λ (exn)
           (log-esterel-debug "~a: main reaction thread done, ~s" (eq-hash-code (current-thread)) exn)
           (log-par-state)
           (set! latest-exn exn)
           (remove-running-thread reaction-thread)
           (loop)))
        )])))

(struct kernel-esterel.rkt::internal-error exn:fail ())
(define (internal-error fmt . args)
  (raise
  (kernel-esterel.rkt::internal-error
   (string-append "kernel-esterel.rkt: internal error: " (apply format fmt args))
   (current-continuation-marks))))
