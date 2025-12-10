#lang racket/base
(require "structs.rkt"
         "compound-signals.rkt"
         syntax/location
         racket/hash
         racket/match
         racket/set
         racket/contract
         racket/list
         racket/symbol
         racket/bool
         (for-syntax racket/base syntax/parse))

;; TODO: it seems like signal death can create race conditions
;;       is there anything we can do about that?

(provide
 (rename-out [-esterel esterel])
 with-signal
 define-signal
 make-global-signal
 par
 suspend
 with-trap
 react!
 in-esterel?
 present?
 signal-value
 signal?
 atomic-signal?
 signal-index
 atomic-signal-combine
 emit
 pause
 exit-trap
 trap?
 exn:fail:not-constructive?
 (struct-out exn:fail:instantaneous-loop)
 esterel?
 get-instant-number

 no-value-provided emit-check-and-error

 ;; private bindings provided for rhombus layer
 mk-signal/args
 esterel/proc
 par/proc
 with-trap/proc
 run-and-kill-signals!
 no-init
 debug-when-must
 exec)


#|

When starting an `esterel`, we create a new thread that has
the state of all the emitted signals, the `instant loop` thread.
Then we also put the argument to `esterel` in its own thread,
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

To handle instantaneous reaction to absence, we block on
unemitted signals. When all threads are blocked, then we know
that we cannot make progress in "must" mode (ie normal execution
of the program), so we switch to "can" mode. We remember where
we are by grabbing continuations for all the threads and then
start exploring to see which signals can be emitted. To explore,
we systematically consider all possible signal settings (ie explore
2^n ways to continue from this point if there are n signals that
the computation is blocked on) and record which signals were
emitted. Any signal that doesn't get emitted during this process
cannot be emitted, so we set it to absent (and continue the
computation, back in "must" mode from where we saved all the
continuations).

There is a subtle point with "dynamically created" signals. That is,
when a signal is created during a "can" exploration, it might be
important that we detect it was not emitted in order to make
progress in the computation. So the question is how do we know
if this signal is the "same" signal for other can exploration
or for must execution? The way we do this is to collect the source
location of the signal when it is created and pair that with a
counter that is specific to the instant where it is being
used. Then, we can reset the counter back to the same
value for can explorations and subsequent must evaluation.
|#

(define-logger esterel)
(define-logger esterel-par)

(struct checkpoint-request (resp-chan))
(define esterel-prompt-tag (make-continuation-prompt-tag 'esterel))

(define current-signal-table (make-parameter #f))
(define (in-esterel?) (and (current-signal-table) #t))

(begin-for-syntax
  (define-splicing-syntax-class signal-name
    #:attributes (name init combine-proc memoryless)
    (pattern
      (~seq name:id #:combine combine)
      #:declare combine
      (expr/c #'(-> any/c any/c any/c)
              #:name "the #:combine argument")
      #:attr combine-proc #'combine.c
      #:attr init #'no-init
      #:attr memoryless #'#f)
    (pattern
      (~seq name:id #:init init:expr #:combine combine)
      #:declare combine
      (expr/c #'(-> any/c any/c any/c)
              #:name "the #:combine argument")
      #:attr combine-proc #'combine.c
      #:attr memoryless #'#f)
    (pattern
      (~seq name:id #:memoryless #:init init:expr #:combine combine)
      #:declare combine
      (expr/c #'(-> any/c any/c any/c)
              #:name "the #:combine argument")
      #:attr combine-proc #'combine.c
      #:attr memoryless #'#t)
    (pattern
      (~seq name:id #:single)
      #:attr init #'no-init
      #:attr combine-proc #''single
      #:attr memoryless #'#f)
    (pattern
      name:id
      #:attr init #'no-init
      #:attr combine-proc #'#f
      #:attr memoryless #'#f)))

(define-values (no-init no-init?)
  (let ()
    (struct no-init ())
    (values (no-init) no-init?)))
 
(define-syntax (with-signal stx)
  (syntax-parse stx
    [(_ (signal:signal-name ...)
        body:expr ...+)
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(signal.name ...)))
     "duplicate variable name"
     #`(let ([srcloc #,(syntax/loc stx (quote-srcloc))])
         (let ([signal.name
                (mk-signal/args 'signal.name
                                signal.init
                                signal.combine-proc
                                (cons 'signal.name srcloc)
                                signal.memoryless
                                )] ...)
           (run-and-kill-signals! (set signal.name ...) (λ () body ...))))]))

(define-for-syntax (assert-top-level stx)
  (unless (member (syntax-local-context) '(module module-begin top-level))
    (raise-syntax-error
     #f
     (string-append
      "illegal syntax;\n"
      " may be used only at the top-level of a module\n"
      " or at the interactive top-level")
     stx)))


(define-syntax (define-signal stx)
  (syntax-parse stx
    [(_ signal:signal-name ...)
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(signal.name ...)))
     "duplicate variable name"
     (assert-top-level stx)
     #`(begin
         (define srcloc (quote-srcloc #,stx))
         (define signal.name
           (mk-signal/args 'signal.name
                           signal.init
                           signal.combine-proc
                           (cons 'signal.name srcloc)
                           signal.memoryless
                           )) ...)]))

(define (make-global-signal name #:init [init no-init] #:combine [combine #f] #:memoryless? [memoryless? #f])
  (mk-signal/args (string->symbol name) init combine #f memoryless?))

(define (mk-signal/args name init combine src memoryless?)
  (define n (symbol->immutable-string name))
  (define id
    ;; the identity of a signal, when we're in an instant,
    ;; is eq-like in that we increment a counter for each
    ;; one, but we arrange to make sure that subsequent
    ;; runs in must mode get the same signal as an earlier
    ;; can run got by including the thread identities and
    ;; the state of the signals during that can run
    (cond
      [(current-signal-table)
       =>
       (λ (signal-table)
         (define new-signal-chan (signal-table-new-signal-chan signal-table))
         (define resp (make-channel))
         (channel-put new-signal-chan resp)
         (cons (channel-get resp) src))]
      [else #f]))
  (if memoryless?
      (memoryless-signal n id init combine)
      (atomic-signal n id init combine)))

(define (run-and-kill-signals! s bodies)
  (define signal-table (current-signal-table))
  (cond
    [signal-table
     (with-continuation-mark suspend-mark s
       (let ([vals (call-with-values bodies list)])
         (channel-put (signal-table-signal-dead-chan signal-table) s)
         (apply values vals)))]
    [else
     (bodies)]))

(define no-value-provided (gensym 'no-value-provided))
(define (emit a-signal [value no-value-provided])
  (define signal-table (current-signal-table))
  (define no-value-provided? (equal? value no-value-provided))
  (emit-check-and-error 'emit a-signal value)
  (define resp-chan (make-channel))
  (channel-put (signal-table-emit-chan signal-table) (vector a-signal no-value-provided? value resp-chan))
  (match (channel-get resp-chan)
    [(signal-suspended)
     (if (equal? value no-value-provided)
         (error 'emit "signal is suspended\n  signal: ~e" a-signal)
         (error 'emit "signal is suspended\n  signal: ~e\n  value: ~e" a-signal value))]
    
    [(signal-ready-and-emitted ready-value)
     (error 'signal-value "emission of a ready signal\n  signal: ~s\n  ready value: ~e\n  emitted value: ~e"
            a-signal
            ready-value
            value)]
    [(signal-single-emitted-twice previous-value)
     (error 'signal-value "multiple emission of a single signal\n  signal: ~s\n  value: ~e\n  value: ~e"
            a-signal
            previous-value
            value)]
    ['dead
     (if (equal? value no-value-provided)
         (error 'emit (string-append "signal dead;\n"
                                     " the dynamic extent of the `with-signal` has ended\n"
                                     "  signal: ~e")
                a-signal)
         (error 'emit (string-append "signal dead;\n"
                                     " the dynamic extent of the `with-signal` has ended\n"
                                     "  signal: ~e\n"
                                     "  value: ~e")
                a-signal
                value))]
    [(? void?) (void)]))
(define (emit-check-and-error name a-signal value)
  (define no-value-provided? (equal? value no-value-provided))
  (if no-value-provided?
      (when (atomic-signal-combine a-signal)
        (error name
               (string-append
                "signal emitted with no value but has a combination function\n"
                "  signal: ~e\n"
                "  value: ~e")
               a-signal value))
      (unless (atomic-signal-combine a-signal)
        (error name
               (string-append
                "signal emitted with a value but has no combination function\n"
                "  signal: ~e\n"
                "  value: ~e")
               a-signal value))))
  
(define (present? a-signal #:pre [pre 0])
  (signal-value/present? #t a-signal pre #f))

(define (signal-value a-signal #:pre [pre 0] #:can [can-result #f])
  (when (and (zero? pre)
             (not can-result))
    (raise-arguments-error 'signal-value
                           "#:can argument missing;\n when #:pre is 0 (the default),\n #:can cannot be #f (also the default)"
                           "signal" a-signal
                           "#:pre" pre
                           "#:can" can-result))
  (signal-value/present? #f a-signal pre can-result))

(define (signal-value/present? is-present? a-signal pre can-result)
  (define signal-table (current-signal-table))
  (define resp-chan (make-channel))
  (unless (<= pre (signal-table-pre-count signal-table))
    (error (if is-present? 'present? 'signal-value)
           "#:pre argument too large\n  maximum: ~a\n  given: ~a"
           (signal-table-pre-count signal-table)
           pre))
  (channel-put (signal-table-signal-presence/value-chan signal-table)
               (vector is-present? a-signal (current-thread) resp-chan pre))
  (let loop ([resp-chan resp-chan])
    (define maybe-val (channel-get resp-chan))
    (match maybe-val
      [(? checkpoint-request?)
       (cond
         [can-result
          (channel-put (checkpoint-request-resp-chan maybe-val)
                       (λ (resp-chan)
                         (define bogus-value (channel-get resp-chan))
                         (for ([a-signal (in-set can-result)])
                           ;; we make up a value here to emit but that should
                           ;; be okay as the values of valued signals are ignored
                           ;; when we're in can mode
                           (if (atomic-signal-combine a-signal)
                               (emit a-signal 'bogus-signal-value-that-we-made-up)
                               (emit a-signal)))
                         (pause)))
          (loop resp-chan)]
         [else
          (loop
           (call/cc
            (λ (k)
              (channel-put (checkpoint-request-resp-chan maybe-val) k)
              resp-chan)
            esterel-prompt-tag))])]
      [(signal-suspended)
       (error (if is-present? 'present? 'signal-value)
              "signal is suspended\n  signal: ~e" a-signal)]
      [(signal-never-before-emitted)
       (define init (atomic-signal-init a-signal))
       (if (no-init? init)
           (error 'signal-value
                  "signal has never been emitted\n  signal: ~e" a-signal)
           init)]
      [else maybe-val])))

(struct signal-never-before-emitted ())
(struct signal-suspended ())
(struct signal-ready-and-emitted (the-value))
(struct signal-single-emitted-twice (other-value))

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
  (unless (in-esterel?) (error 'par "not in `esterel`"))
  (define s (make-semaphore 0))
  (define before-par-trap-counter (get-current-trap-counter))
  (define result-chans+children-threads
    (for/set ([thunk (in-list thunks)])
      ;; par-child-result-chan : channel[(or/c esterel-thread-value? trap+vals? exn?)]
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
             ;; is it possible we get asked for the same continuation multiple times?
             ;; should we save them, if so?
             (call/cc
              (λ (k)
                (channel-put checkpoint-resp-chan-or-final-result (vector before-par-trap-counter k))
                (values pending-result-chans+par-threads checkpoint-or-par-result-chan))
              esterel-prompt-tag))
           (when (set-empty? pending-result-chans+par-threads)
             (internal-error "asked for a par checkpoint with no children"))
           (loop new-pending-result-chans+par-threads new-checkpoint-or-par-result-chan)]
          [(? (or/c trap+vals? exn?))
           (unless (set-empty? pending-result-chans+par-threads)
             (internal-error "exiting the par but still have children.1 ~s ~s ~s"
                             (current-thread)
                             checkpoint-resp-chan-or-final-result
                             pending-result-chans+par-threads))
           (exit-trap/internal checkpoint-resp-chan-or-final-result)]
          [(esterel-thread-value vals)
           (unless (set-empty? pending-result-chans+par-threads)
             (internal-error "exiting the par but still have children.2 ~s ~s"
                             (current-thread) pending-result-chans+par-threads))
           vals])))
     (for/list ([result-chan+pending-par-thread (in-set pending-result-chans+par-threads)])
       (match-define (cons result-chan pending-par-thread) result-chan+pending-par-thread)
       (handle-evt
        result-chan
        (λ (par-child-result)
          (channel-put (signal-table-par-partly-done-chan signal-table)
                       (vector pending-par-thread par-child-result))
          (loop (set-remove pending-result-chans+par-threads result-chan+pending-par-thread)
                checkpoint-or-par-result-chan)))))))

;; we didn't save the parameterization at the point of the pause/present?/par
;; and so we cannot restore it here; not sure if this is important or not, tho!
;; this function creates the par children thread when a par is first encountered
;; and it also creates the threads for when we fall back to a previous state
;; to run in can mode. It doesn't create the main
;; esterel thread but probably things should be cleaned up a bit so it can.
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
                  (esterel-thread-value
                   (set
                    (call-with-continuation-prompt
                     thunk
                     esterel-prompt-tag)))))))))]
      [else
       (λ ()
         (call-with-continuation-prompt
          thunk
          esterel-prompt-tag))]))
  ;; this renaming doesn't work when rebuilding the threads currently
  (define thunk-name (object-name thunk))
  (thread (if thunk-name
              (procedure-rename thunk-to-run thunk-name)
              thunk-to-run)))

;; values : (set/c any/c)
;; these are the values of the par
(struct esterel-thread-value (values))

(define (raise-argument->exn x)
  (if (exn? x)
      x
      (make-exn:fail (format "uncaught exn: ~e" x)
                     (current-continuation-marks))))

;; outermost-trap :
;;    (or/c esterel-thread-value? trap+val? exn?)
;;    (or/c esterel-thread-value? trap+val? exn?)
;; -> (or/c esterel-thread-value? trap+val? exn?)
(define (outermost-trap t1 t2)
  (cond
    [(or (exn? t1) (exn? t2))
     (if (exn? t1) t1 t2)]
    [(and (trap+vals? t1) (trap+vals? t2))
     (match-define (trap+vals (trap name1 counter1 escape1) vals1) t1)
     (match-define (trap+vals (trap name2 counter2 escape2) vals2) t2)
     (cond
       [(= counter1 counter2)
        (trap+vals (trap+vals-trap t1) (set-union vals1 vals2))]
       [(< counter1 counter2) t1]
       [else t2])]
    [(or (trap+vals? t1) (trap+vals? t2))
     (if (trap+vals? t1) t1 t2)]
    [else
     (esterel-thread-value
      (set-union
       (esterel-thread-value-values t1)
       (esterel-thread-value-values t2)))]))

(define (pause)
  (define signal-table (current-signal-table))
  (define resp-chan (make-channel))
  (channel-put (signal-table-pause-chan signal-table)
               (vector (current-thread) resp-chan))
  (define val (channel-get resp-chan))
  (cond
    [(exn? val) (raise val)]
    [(trap+vals? val) (exit-trap/internal val)]
    [else
     (define suspend? (handle-suspension signal-table val))
     (cond
       [suspend?
        (pause)]
       [else
        (void)])]))

;; get-suspend-info : marks -> suspend-info
(define (get-suspend-info thds)
  (for/fold ([suspended? #f]
             [suspended-signals (set)])
            ([thd (in-list thds)])
    (define marks
      (continuation-mark-set->list
       (continuation-marks thd)
       suspend-mark))
    ;; work from the outermost suspend inwards (hence
    ;; the reverse of the marks) so that we can tell
    ;; when we're traversing an already suspended part
    ;; of the continuation
    (let loop ([items (reverse marks)]
               [suspended? suspended?]
               [suspended-signals suspended-signals])
      (match items
        ['()
         (values suspended? suspended-signals)]
        [(cons (? procedure? suspend-proc) items)
         (loop items
               ;; if we're already suspended, take care
               ;; to avoid running the suspend-proc, as
               ;; it is code that's supposed to be
               ;; suspended (so may test a suspended signal
               ;; or emit a suspended one)
               (or suspended? (suspend-proc))
               suspended-signals)]
        [(cons (? set? signals) items)
         (loop items
               suspended?
               (if suspended?
                   (set-union signals suspended-signals)
                   suspended-signals))]))))

(define (handle-suspension signal-table thds)
  (define-values (suspend? suspended-signals) (get-suspend-info thds))
  (unless (set-empty? suspended-signals)
    (define resp-chan (make-channel))
    (channel-put (signal-table-suspended-signals-chan signal-table)
                 (cons suspended-signals resp-chan))
    (define resp (channel-get resp-chan))
    (when resp
      (error 'suspend "suspended signal was used\n  signal: ~e"
             resp)))
  suspend?)

(define-syntax (suspend stx)
  (syntax-case stx ()
    [(_ e-body e-when)
     #`(suspend/proc #,(syntax/loc #'e-body (λ () e-body))
                     #,(syntax/loc #'e-when (λ () e-when)))]))

(define suspend-mark (gensym 'suspend))
(define (suspend/proc body signal-thunk)
  (unless (in-esterel?) (error 'suspend "not in `esterel`"))
  (with-continuation-mark suspend-mark signal-thunk
    ;; we don't want the body to be in tail position
    (begin0
      (body)
      (void))))

(define-syntax (with-trap stx)
  (syntax-parse stx
    [(_ t:identifier body ...+)
     #'(with-trap/proc 't (λ (t) body ...))]))

(define (build-trap name escape)
  (trap name (get-current-trap-counter) escape))

(define (get-current-trap-counter) (or (continuation-mark-set-first #f trap-counter-mark) 0))
(define trap-counter-mark (gensym 'trap-counter))
(define trap-start-of-par-mark (gensym 'trap-counter))

(define (with-trap/proc trap-name body)
  (let/ec escape
    (define trap (build-trap trap-name escape))
    (define counter (trap-counter trap))
    (with-continuation-mark trap-counter-mark (+ counter 1)
      ;; we don't need to add something to block
      ;; multiple uses of the mark because the let/ec
      ;; introduces a non-tail context (for repeated
      ;; `with-mark`s).
      (body trap))))

(define (exit-trap trap [val (void)])
  (exit-trap/internal (trap+vals trap (set val))))

(define (exit-trap/internal exn-or-trap+vals)
  (define start-of-par-counter (continuation-mark-set-first #f trap-start-of-par-mark))
  (cond
    [(or (not start-of-par-counter)
         ;; the start of the par value will be the same as
         ;; the counter used for the first trap inside the par
         ;; so this should be an inclusive comparison
         (exn? exn-or-trap+vals)
         (<= (vector-ref start-of-par-counter 1) (trap-counter (trap+vals-trap exn-or-trap+vals))))
     ;; here the trap doesn't span a par, so we can just escape
     (if (exn? exn-or-trap+vals)
         (raise exn-or-trap+vals)
         ((trap-escape (trap+vals-trap exn-or-trap+vals)) (trap+vals-vals exn-or-trap+vals)))]
    [else
     ;; here we tell the enclosing par that a trap has happened
     ((vector-ref start-of-par-counter 0) exn-or-trap+vals)]))

(define-syntax (debug-when-must stx)
  (syntax-parse stx
    [(_ e1 e2 ...)
     #'(debug-when-must/proc (λ () e1 e2 ...))]))
(define (debug-when-must/proc thunk)
  (define signal-table (current-signal-table))
  (unless signal-table (error 'debug-when-must "not in `esterel`"))
  (when (in-must-mode? signal-table)
    (thunk)))

(define (in-must-mode? signal-table)
  (define c (make-channel))
  (channel-put (signal-table-when-must-chan signal-table) c)
  (channel-get c))

(define-syntax (exec stx)
  (syntax-parse stx
    [(_ esterel:id ([x:id x-e:expr] ...)
        body-e1:expr body-e2:expr ...
        (~optional (~seq #:kill kill-e1:expr kill-e2:expr ...)
                   #:defaults ([kill-e1 #'(void)] [(kill-e2 1) '()]))
        (~optional (~seq #:suspend suspend-e1:expr suspend-e2:expr ...)
                   #:defaults ([suspend-e1 #'(void)] [(suspend-e2 1) '()]))
        (~optional (~seq #:resume resume-e1:expr resume-e2:expr ...)
                   #:defaults ([resume-e1 #'(void)] [(resume-e2 1) '()])))
     #'(exec/proc
        (λ (esterel)
          (let ([x x-e] ...)
            (values
             (λ () body-e1 body-e2 ...)
             (λ () kill-e1 kill-e2 ...)
             (λ () suspend-e1 suspend-e2 ...)
             (λ () resume-e1 resume-e2 ...)))))]))

(define (exec/proc mk)
  (define signal-table (current-signal-table))
  (unless signal-table (error 'exec "not in `esterel`"))
  (define-values (body-thunk kill-thunk suspend-thunk resume-thunk)
    (mk (signal-table-the-esterel signal-table)))
  (define exec-finished (make-channel))
  (thread (λ ()
            (body-thunk)
            (channel-put exec-finished (void))))
  (define (start-a-new-pause)
    (define resp-chan (make-channel))
    (channel-put (signal-table-pause-chan signal-table)
                 (vector (current-thread) resp-chan))
    resp-chan)
  (let loop ([suspended? #f]
             [finished? #f]
             [resp-chan (start-a-new-pause)])
    (sync
     (handle-evt
      resp-chan
      (λ (val)
        (cond
          [(exn? val)
           (unless finished? (when (in-must-mode? signal-table) (kill-thunk)))
           (raise val)]
          [(trap+vals? val)
           (unless finished? (when (in-must-mode? signal-table) (kill-thunk)))
           (exit-trap/internal val)]
          [else
           (define suspend? (handle-suspension signal-table val))
           (cond
             [(in-must-mode? signal-table)
              (unless finished?
                (unless (equal? suspend? suspended?)
                  (set! suspended? suspend?)
                  (if suspend?
                      (suspend-thunk)
                      (resume-thunk))))
              (cond
                [(and (not suspend?) finished?)
                 (void)]
                [else
                 (loop suspend? finished? (start-a-new-pause))])]
             [else
              ;; `handle-suspension` might return in can mode;
              ;; in that case, we just pretend to be a pause
              ;; and don't communicate with the running exec.
              (loop suspended? finished? (start-a-new-pause))])])))
     (handle-evt
      exec-finished
      (λ (_)
        (loop suspended? #t resp-chan))))))

;; returns #f if we are in can mode and an
;; natural number if we're in must mode
(define (get-instant-number)
  (define c (make-channel))
  (channel-put (signal-table-instant-number-chan (current-signal-table)) c)
  (channel-get c))

(struct signal-table (new-signal-chan
                      signal-presence/value-chan
                      signal-dead-chan
                      emit-chan
                      par-start-chan par-partly-done-chan
                      pause-chan instant-chan
                      react-thread-done-chan
                      suspended-signals-chan
                      when-must-chan
                      instant-number-chan
                      pre-count
                      [the-esterel #:mutable]))
  
(struct esterel (signal-table) #:mutable)
(define-syntax (-esterel stx)
  (syntax-parse stx
    [(_ #:pre pre-expr:expr e1:expr e2:expr ...)
     #'(esterel/proc pre-expr (λ () e1 e2 ...))]
    [(_ e1:expr e2:expr ...)
     #'(esterel/proc 0 (λ () e1 e2 ...))]))
(define (esterel/proc pre-count thunk)
  (define the-signal-table
    (signal-table (make-channel) (make-channel) (make-channel) (make-channel)
                  (make-channel) (make-channel) (make-channel) (make-channel)
                  (make-channel) (make-channel) (make-channel) (make-channel)
                  pre-count #f))
  (thread (λ () (run-esterel-thread pre-count thunk the-signal-table)))
  (define me (esterel the-signal-table))
  (set-signal-table-the-esterel! the-signal-table me)
  me)

(define (react! an-esterel #:emit [signals-to-emit '()])
  (define signal-table (esterel-signal-table an-esterel))
  (define maybe-signals
    (match signal-table
      [(cons 'non-constructive _)
       signal-table]
      [(? exn?)
       signal-table]
      [else
       (define instant-complete-chan (make-channel))
       (channel-put (signal-table-instant-chan signal-table)
                    (cons signals-to-emit instant-complete-chan))
       (define maybe-signals (channel-get instant-complete-chan))
       (when (or (and (pair? maybe-signals)
                      (equal? (car maybe-signals) 'non-constructive))
                 (exn? maybe-signals))
         (set-esterel-signal-table! an-esterel maybe-signals))
       maybe-signals]))
  (match maybe-signals
    [(cons 'non-constructive info)
     (define details
       (cond
         [(= 1 (set-count info))
          (format
           (string-append
            " the signal blocking progress can be emitted\n"
            "  signal: ~e")
           (set-first info))]
         [else
          (format
           (string-append
            " all of the signals blocking progress can be emitted\n"
            "  signals:~a")
           (apply string-append
                  (for/list ([a-signal (in-set info)]
                             [i (in-naturals)])
                    (format " ~e" a-signal))))]))
     (raise
      (exn:fail:not-constructive
       (string-append
        "react!: the program is not constructive\n"
        details)
       (current-continuation-marks)))]
    [(? exn?) (raise maybe-signals)]
    [#f (error 'react! "an instant is already running")]
    [_ maybe-signals]))

(struct exn:fail:not-constructive exn:fail ())
(struct exn:fail:instantaneous-loop exn:fail ())

(define (run-esterel-thread pre-count thunk the-signal-table)
  (define first-instant-sema (make-semaphore 0))
  (define esterel-thread
    (let ([first-instant-sema first-instant-sema])
      (parameterize ([current-signal-table the-signal-table])
        (define (esterel-thread-thunk)
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
           esterel-prompt-tag))
        (thread esterel-thread-thunk))))
  (match-define (signal-table new-signal-chan signal-presence/value-chan
                              signal-dead-chan emit-chan
                              par-start-chan par-partly-done-chan
                              pause-chan instant-chan
                              react-thread-done-chan
                              suspended-signals-chan
                              when-must-chan
                              instant-number-chan
                              pre-count
                              _)
    the-signal-table)

  ;; tracks the values of signals in previous instants
  (define/contract signals-pre
    (listof (set/c atomic-signal?))
    '())
  (define/contract signal-values-pre
    (listof (hash/c atomic-signal? any/c #:flat? #t #:immutable #t))
    '())


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

  ;; the can struct tracks our progress in can mode; each time progress
  ;; is blocked on some number of signals, we grab the continuations
  ;; where we are and save them to return to them, thereby searching for
  ;; emits that are downstream to determine what Can should be.
  ;;
  ;; emits : the signals that can be emitted (that we've learned about so far)
  ;; considered-signals: the signals that we were blocked in, for any past stage
  ;;    that is, as each stage finishes we'll add in the ordered signals
  ;;    from that stage so by the end of can mode we know what ever got blocked on
  ;;    as candidates for signals that we can set to false
  ;; stages : each time the computation is blocked, we add another element to
  ;;    this list; when we collect all the emitted signals from exploring
  ;;    past the signals in that stage, we drop the stage from the list
  (struct/contract can ([emits (set/c atomic-signal?)]
                        [considered-signals (set/c atomic-signal?)]
                        [stages (non-empty-listof (λ (x) (can-stage? x)))])
                   #:transparent)

  ;; newly-ready : the signals that we were blocked on with `signal-value`;
  ;;    unblock to see if there are emits past them (this doesn't invoke the
  ;;    continuation, but just returns what the programmer wrote)
  ;; starting-point : these are the continuations for the blocked state of this stage
  ;; ordered-signals : the signals that were blocked on at starting-point
  ;;    these are possibly compound signals as we block on the entire
  ;;    expression (when it cannot be evaluated)
  ;; signal-states : the bits in this integer tell us what the signal values for
  ;;    the next exploration starting from the starting point should be
  (struct/contract can-stage ([newly-ready (set/c (and/c atomic-signal? atomic-signal-combine))]
                              [starting-point (λ (x) (starting-point? x))]
                              [ordered-signals (listof signal?)]
                              [signal-states (and/c exact? integer?)])
                   #:transparent)

  (define/contract mode
    (or/c #f    ;; we're in Must mode
          can?) ;; we're in Can mode
    #f)

  ;; inc-signal-states : can-stage -> can-stage
  (define (inc-signal-states a-can-stage)
    (struct-copy can-stage a-can-stage
                 [signal-states (+ (can-stage-signal-states a-can-stage) 1)]))

  ;; signal-states-done? : can-stage -> boolean
  (define (signal-states-done? a-can-stage)
    ;; we need to run (expt 2 (length (length (can-stage-ordered-signals a-can-stage))))
    ;; iterations to be sure we've tried every combination of absent and present
    ;; for the two-choice signals, going from all zeros to all ones
    (= (- (expt 2 (length (can-stage-ordered-signals a-can-stage))) 1)
       (can-stage-signal-states a-can-stage)))

  ;; signal-states->hash : can-stage -> hash[signal -o> boolean?]
  (define (signal-states->hash a-can-stage)
    (match-define (can-stage newly-ready starting-point ordered-signals signal-states) a-can-stage)
    (for/hash ([a-signal (in-list ordered-signals)]
               [i (in-naturals)])
      (define on? (bitwise-bit-set? signal-states i))
      (values a-signal on?)))
  
  ;; add-emitted-signal : can? signal? -> can?
  ;; adds `a-signal` as emitted to `a-can`
  (define (add-emitted-signal a-can a-signal)
    (struct-copy can a-can
                 [emits (set-add (can-emits a-can) a-signal)]))
    
  ;; must-state holds the state of the instant at the point where we
  ;; switched form must mode to can mode (so that we can return to
  ;; that state when we are done with can)
  (struct must-state (new-signal-counter
                      signal-status
                      signal-ready
                      signal-value
                      dead-signals
                      latest-exn
                      presence-waiters
                      value-waiters
                      paused-threads
                      par-parents
                      parent->par-state
                      esterel-thread))

  (define instant-number 0)

  ;; saved-must-state : (or/c #f must-state?)
  ;; #f when we're in must mode and must-state? when we're in can mode
  (define saved-must-state #f)

  ;; new-signal-counter : natural?
  ;; used to control the equivalence relation of signals so when we
  ;; roll back we can create the "same" signals as we did in a previous run
  (define new-signal-counter 0)
  
  ;; if a signal isn't mapped, its status isn't yet known
  ;; if it is #t, the signal has been emitted
  ;; if it is #f, the signal is not going to be emitted this instant
  ;; this applies to both value-carrying signals and regular ones
  (define/contract signal-status
    (hash/c atomic-signal? (or/c boolean? 'suspended) #:flat? #t #:immutable #t)
    (hash))

  ;; signals in `signal-ready` will not be emitted again this instant
  (define/contract signal-ready
    (set/c (and/c atomic-signal? atomic-signal-combine))
    (set))

  ;; if a signal is mapped in `signal-value`, then
  ;; a) it has been emitted (but there may be more emits coming) and
  ;; b) it has a `combine` function
  ;; it is mapped to its current value
  (define/contract signal-value
    (hash/c atomic-signal? any/c #:flat? #t #:immutable #t)
    (hash))

  ;; these are all the signals that can no longer be emitted because the
  ;; dynamic extent of their `with-signal` declaration is has ended
  (define/contract dead-signals
    (set/c atomic-signal?)
    (set))

  ;; this is all of the exceptions that were raised
  ;; when running the current instant
  (define/contract raised-exns
    (listof exn?)
    '())

  (struct blocked-thread (thread resp-chan) #:transparent)

  ;; threads that are blocked, waiting for a signal's presence to be decided
  (define/contract presence-waiters
    (hash/c signal? (non-empty-listof blocked-thread?) #:flat? #t #:immutable #t)
    (hash))
  (define (add-presence-waiter! a-signal the-thread resp-chan)
    (define a-blocked-thread (blocked-thread the-thread resp-chan))
    (define new-waiters (cons a-blocked-thread (hash-ref presence-waiters a-signal '())))
    (set! presence-waiters (hash-set presence-waiters a-signal new-waiters)))

  ;; -> (set/c hash/c[blocked-thread? -o> boolean?])
  ;; recheck all of the signals that we're waiting for to see if we
  ;; can make progress on any of them, return the ones that're no longer blocked
  ;; effect: changes presence-waiters
  (define (update-presence-waiters-following-emission)
    (define-values (new-presence-waiters newly-unblocked)
      (for/fold ([new-presence-waiters (hash)]
                 [newly-unblocked-threads (hash)])
                ([(a-signal blocked-threads) (in-hash presence-waiters)])
        (define presence-result (compute-signal-presence a-signal signal-status))
        (cond
          [(set? presence-result)
           (values (hash-set new-presence-waiters a-signal blocked-threads)
                   newly-unblocked-threads)]
          [else
           (values new-presence-waiters
                   (hash-set newly-unblocked-threads blocked-threads presence-result))])))
    (set! presence-waiters new-presence-waiters)
    newly-unblocked)

  ;; find-presence-waiter : thread -> (cons signal blocked-thread) or #f
  ;; returns the signal that `thread` is blocked on, or #f it is isn't blocked on a signal
  (define (find-presence-waiter thread) (find-waiter presence-waiters thread))

  ;; threads that are blocked, waiting for a signal's value to be decided
  (define/contract value-waiters
    (hash/c atomic-signal? (non-empty-listof blocked-thread?) #:flat? #t #:immutable #t)
    (hash))
  (define (add-value-waiter! a-signal the-thread resp-chan)
    (define a-blocked-thread (blocked-thread the-thread resp-chan))
    (define new-waiters (cons a-blocked-thread (hash-ref value-waiters a-signal '())))
    (set! value-waiters (hash-set value-waiters a-signal new-waiters)))

  ;; find-value-waiter : thread -> (cons signal blocked-thread) or #f
  ;; returns the signal that `thread` is blocked on, or #f it is isn't blocked on a signal
  (define (find-value-waiter thread) (find-waiter value-waiters thread))

  ;; helper for find-value-waiter and find-presence-waiter
  (define (find-waiter the-waiters thread)
    (for*/or ([(signal blocked-threads) (in-hash the-waiters)]
              [a-blocked-thread (in-list blocked-threads)])
      (and (equal? (blocked-thread-thread a-blocked-thread) thread)
           (cons signal a-blocked-thread))))

  ;; each paused thread is blocked on the corresponding channel
  ;; when a par's threads are all paused, the parent thread does
  ;; *not* end up here (even though it is morally paused)
  ;; also, threads that're currently running an exec are here too
  (define/contract paused-threads
    (hash/c thread? channel? #:flat? #t #:immutable #t)
    (hash))

  ;; (set/c hash)
  ;; all of the threads in the instant that are:
  ;;    - not blocked on a signal
  ;;    - not paused
  ;;    - not a par-parent thread
  (define running-threads (set esterel-thread))
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
        (internal-error
         "adding a running thread (via add-running-threads) but it was already running ~s"
         t))
      (when (hash-has-key? parent->par-state t)
        (internal-error
         "adding a running thread (via add-running-threads) but it was a par parent ~s"
         t)))
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
  ;; - presence-waiting is the set of children threads that are blocked waiting
  ;;   on a call to present? for a signl
  ;; - value-waiting is the set of children threads that are blocked waiting on
  ;;   a call to `signal-value`
  ;; - paused is the set of paused children threads
  ;; - active is the set of threads that are running
  ;; - trap is the highest trap (or exn) that any of these children have
  ;;   exited to, or #f if none of them have exited to a trap
  (struct par-state (result/checkpoint-chan presence-waiting value-waiting paused active trap) #:transparent)
  (define/contract parent->par-state
    (hash/c thread? (struct/c par-state
                              channel?
                              (set/c thread?)
                              (set/c thread?)
                              (set/c thread?)
                              (set/c thread?)
                              (or/c esterel-thread-value? trap+vals? exn?))
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
    (log-esterel-par-debug
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
          (match-define (par-state result/checkpoint-chan presence-waiting value-waiting paused active a-trap)
            a-par-state)
          (fprintf sp "par parent: ~s; trap: ~a" thread a-trap)
          (display nl sp)
          (fprintf sp "  ps?: ~s" presence-waiting)
          (display nl sp)
          (fprintf sp "  val: ~s" value-waiting)
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


  ;; get-signals-value : signal boolean -> any/c
  ;; returns what the response to present?/signal-value should be for `a-signal`
  ;;   if the signal hasn't been emitted yet and we ask for its value in
  ;;   the first instant, then signal-never-before-emitted will trigger an
  ;;   error at the call site of signal-value or will return the init value
  (define (get-signals-value a-signal)
    ;; if the signal has a value in this instant, take the value from
    ;; `signal-value` if not, take the value from the previous instant
    ;; (as values are carried forward, we do not need to look further back)
    ;; NB: the signal may be present but also not have a value in `signal-value`
    ;;     because we may be in can mode and we're treating presence and
    ;;     value as separate components. This cannot happen in must mode.
    (hash-ref signal-value a-signal
              (λ ()
                (cond
                  [(memoryless-signal? a-signal)
                   (atomic-signal-init a-signal)]
                  [else
                   (if (pair? signal-values-pre)
                       (hash-ref (car signal-values-pre)
                                 a-signal
                                 (λ ()
                                   (signal-never-before-emitted)))
                       (signal-never-before-emitted))]))))

  (define (signal-has-a-value? a-signal)
    (or (hash-has-key? signal-value a-signal)
        (and (pair? signal-values-pre)
             (hash-has-key? (car signal-values-pre) a-signal))))

  ;; unblock-presence-threads : (or/c #f (hash[signal -o> boolean?])) -> void
  ;; wakes up all the threads that are blocked on a presence test for
  ;; the signals in `signals-maps` or, if signals-map is #f, wake
  ;; up the ones whose signals now evaluate to a boolean (as signal-status
  ;; has changed since the threads blocked)
  (define (unblock-presence-threads signals-map)
    (for ([(a-signal presence-blocked-threads) (in-hash presence-waiters)])
      (define status (cond
                       [signals-map
                        (cond
                          [(hash-has-key? signals-map a-signal)
                           (hash-ref signals-map a-signal)]
                          [else "not a boolean"])]
                       [else
                        (compute-signal-presence a-signal signal-status)]))
      (when (boolean? status)
        (set! presence-waiters (hash-remove presence-waiters a-signal))
        (for ([a-blocked-thread (in-set presence-blocked-threads)])
          (unblock-a-thread a-blocked-thread status)))))

  ;; unblock-value-threads : (listof signal) -> void
  ;; wakes up all the threads that are blocked on a signal's value for
  ;; the signals in `signals`.
  (define (unblock-value-threads signals)
    (for ([a-signal (in-list signals)])
      (define value-blocked-threads (hash-ref value-waiters a-signal #f))
      (when value-blocked-threads
        (set! value-waiters (hash-remove value-waiters a-signal))
        (define value (get-signals-value a-signal))
        (for ([a-blocked-thread (in-set value-blocked-threads)])
          (unblock-a-thread a-blocked-thread value)))))

  ;; unblock-a-thread : blocked-thread any -> void
  ;; wakes up the thread in `a-blocked-thread` using `value-to-send` to continue
  (define (unblock-a-thread a-blocked-thread value-to-send)
    (match-define (blocked-thread thread resp-chan) a-blocked-thread)
    (channel-put resp-chan value-to-send)
    (add-running-thread thread)
    (define parent-thread (hash-ref par-parents thread #f))
    (when parent-thread
      (define old-par-state (hash-ref parent->par-state parent-thread))
      (define new-par-state
        (struct-copy par-state old-par-state
                     [active (set-add (par-state-active old-par-state) thread)]
                     ;; the thread can't be in both sets so one of these
                     ;; removals won't do anything
                     [presence-waiting
                      (set-remove (par-state-presence-waiting old-par-state) thread)]
                     [value-waiting
                      (set-remove (par-state-value-waiting old-par-state) thread)]))
      (set! parent->par-state (hash-set parent->par-state parent-thread new-par-state))))
  
  ;; save-must-state : -> must-state?
  (define (save-must-state)
    (when saved-must-state (error 'save-must-state "already have a must state saved"))
    (set! saved-must-state
          (must-state new-signal-counter
                      signal-status
                      signal-ready
                      signal-value
                      dead-signals
                      raised-exns
                      presence-waiters
                      value-waiters
                      paused-threads
                      par-parents
                      parent->par-state
                      esterel-thread)))

  (define (restore-must-state)
    (match-define (must-state _new-signal-counter
                              _signal-status
                              _signal-ready
                              _signal-value
                              _dead-signals
                              _raised-exns
                              _presence-waiters
                              _value-waiters
                              _paused-threads
                              _par-parents
                              _parent->par-state
                              _esterel-thread)
      saved-must-state)
    (set! new-signal-counter _new-signal-counter)
    (set! signal-status _signal-status)
    (set! signal-ready _signal-ready)
    (set! signal-value _signal-value)
    (set! dead-signals _dead-signals)
    (set! raised-exns _raised-exns) ;; there cannot be anything there ... right?
    (set! presence-waiters _presence-waiters)
    (set! value-waiters _value-waiters)
    (set! paused-threads _paused-threads)
    (set! par-parents _par-parents)
    (set! parent->par-state _parent->par-state)
    (set! esterel-thread _esterel-thread)
    (set! saved-must-state #f))

  ;; the `starting-point` struct holds information about the starting point
  ;; for running can. we start the computation here with various settings
  ;; of the signals to see what can be emitted (and thus, what cannot be emitted)
  ;; rb-tree : rb-tree?
  ;; signals : (hash/c signal? boolean? #:flat? #t #:immutable #t)
  (struct starting-point (rb-tree new-signal-counter signal-status signal-ready signal-value dead-signals))
  (define (rollback! a-starting-point)
    (match-define (starting-point rb-tree rb-new-signal-counter
                                  rb-signal-status rb-signal-ready
                                  rb-signal-value rb-dead-signals)
      a-starting-point)
    (set! new-signal-counter rb-new-signal-counter)
    (set! signal-status rb-signal-status)
    (set! signal-ready rb-signal-ready)
    (set! signal-value rb-signal-value)
    (set! dead-signals rb-dead-signals)
    (set! raised-exns '())
    (set! presence-waiters (hash))
    (set! value-waiters (hash))
    (set! paused-threads (hash))
    (set! par-parents (hash))
    (set! parent->par-state (hash))
    (set! esterel-thread (rebuild-threads-from-rb-tree rb-tree)))
  (define (get-starting-point)
    (starting-point (build-rb-tree-from-current-state)
                    new-signal-counter signal-status
                    (set-union (list->set (hash-keys value-waiters)) signal-ready)
                    (for/fold ([signal-value signal-value])
                              ([a-signal (in-list (hash-keys value-waiters))])
                      (cond
                        [(hash-has-key? signal-value a-signal) signal-value]
                        [else (hash-set signal-value a-signal
                                        'dummy-value-that-shouldn’t-be-used-anywhere)]))
                    dead-signals))

  (struct rb-tree () #:transparent)

  ;; cont : continuation[listof thread]
  ;; signal-waiting : (set/c rb-tree?)
  ;; active : (set/c rb-tree?)
  ;;     -- `active` are children of the current par
  ;;        that have children; they aren't running
  ;; trap : (or/c trap+vals? exn? #f)
  ;; before-par-trap-counter : natural?
  (struct rb-par rb-tree (cont presence-waiting value-waiting active trap before-par-trap-counter)
    #:transparent)

  ;; signal : signal?
  ;; presence? : boolean?  -- indicates if they are blocked on `present?` or `signal-value`
  ;; cont : continuation[channel]
  (struct rb-blocked rb-tree (signal presence? cont) #:transparent)

  ;; build-rb-tree-from-current-state : -> rb-tree?
  (define (build-rb-tree-from-current-state)
    (define k-chan (make-channel))
    (define a-checkpoint-request (checkpoint-request k-chan))
    (let loop ([thread esterel-thread])
      (cond
        [(hash-has-key? parent->par-state thread)
         (match-define (par-state checkpoint/result-chan presence-waiting value-waiting paused active a-trap)
           (hash-ref parent->par-state thread))
         (channel-put checkpoint/result-chan k-chan)
         (match-define (vector before-par-trap-counter par-continuation) (channel-get k-chan))
         (rb-par
          par-continuation
          (for/set ([child (in-set presence-waiting)])
            (loop child))
          (for/set ([child (in-set value-waiting)])
            (loop child))
          (for/set ([child (in-set active)])
            (loop child))
          a-trap
          before-par-trap-counter)]
        [(hash-has-key? paused-threads thread)
         (error 'build-rb-tree-from-current-state
                "paused threads should be ignored when building the rb tree")]
        [(find-presence-waiter thread)
         =>
         (λ (signal+blocked-thread)
           (match-define (cons signal (blocked-thread thread resp-chan))
             signal+blocked-thread)
           (channel-put resp-chan a-checkpoint-request)
           (rb-blocked signal #t (channel-get k-chan)))]
        [(find-value-waiter thread)
         =>
         (λ (signal+blocked-thread)
           (match-define (cons signal (blocked-thread thread resp-chan))
             signal+blocked-thread)
           (channel-put resp-chan a-checkpoint-request)
           (rb-blocked signal #f (channel-get k-chan)))]
        [else (internal-error "lost a thread ~s" thread)])))

  ;; rebuild-threads-from-rb-tree : rb-tree? -> thread
  ;; returns the new thread that'll run a can exploration
  (define (rebuild-threads-from-rb-tree rb-tree)
    (let loop ([rb-tree rb-tree]
               [par-child-result-chan #f]
               [parent-before-par-trap-counter #f])
      (match rb-tree
        [(rb-par par-cont rb-presence-waiting rb-value-waiting rb-active a-trap before-par-trap-counter)
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
                (define this-par-result-chans+presence-waiting-children
                  (get-result-chans+par-children rb-presence-waiting))
                (define this-par-result-chans+value-waiting-children
                  (get-result-chans+par-children rb-value-waiting))
                (define this-par-result-chans+active-children
                  (get-result-chans+par-children rb-active))
                (define presence-waiting
                  (drop-result-chans this-par-result-chans+presence-waiting-children))
                (define value-waiting
                  (drop-result-chans this-par-result-chans+value-waiting-children))
                (define active (drop-result-chans this-par-result-chans+active-children))
                (for ([child-thread (in-set (set-union presence-waiting value-waiting active))])
                  (set! par-parents (hash-set par-parents child-thread parent-thread)))
                (set! parent->par-state
                      (hash-set parent->par-state
                                parent-thread
                                (par-state checkpoint/result-chan
                                           presence-waiting
                                           value-waiting
                                           ;; we never rebuild paused threads because
                                           ;; can exploration never crosses a pause so
                                           ;; we don't even collect their continuations
                                           (set)
                                           active
                                           a-trap)))
                (semaphore-post sema)
                (par-cont (set-union this-par-result-chans+presence-waiting-children
                                     this-par-result-chans+value-waiting-children
                                     this-par-result-chans+active-children)
                          checkpoint/result-chan)))))
         (semaphore-wait sema)
         par-parent-thread]
        [(rb-blocked a-signal presence? cont)
         (define resp-chan (make-channel))
         (define blocked-thread
           (parameterize ([current-signal-table the-signal-table])
             (make-esterel-thread
              #:before-par-trap-counter parent-before-par-trap-counter
              #:par-child-result-chan par-child-result-chan
              (λ () (cont resp-chan)))))
         (if presence?
             (add-presence-waiter! a-signal blocked-thread resp-chan)
             (add-value-waiter! a-signal blocked-thread resp-chan))
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
    (match-define (par-state result/checkpoint-chan presence-waiting value-waiting paused active a-trap)
      (hash-ref parent->par-state parent-thread))

    (when (and (set-empty? presence-waiting)
               (set-empty? value-waiting)
               (set-empty? active))
      ;; here we know that none of the threads in the par are going to do
      ;; anything more in this instant. time to close the par up
      (cond
        [(set-empty? paused)
         ;; if there are no paused or execing threads, then we know that the result
         ;; of this par is the trap (which might be #f meaning no actual
         ;; trap); send it to the parent thread
         (channel-put result/checkpoint-chan a-trap)
         (set! parent->par-state (hash-remove parent->par-state parent-thread))
         ;; also, shut down all of the execs
         (add-running-thread parent-thread)]
        [(or (exn? a-trap) (trap+vals? a-trap))
         ;; here we have a trap (or an exn) and there is at least one paused or
         ;; execing thread; we need to tell all the paused threads to exit to the trap
         ;; when that finishes we'll be back here to close up the par itself
         (unpause-to-trap parent-thread (or a-trap (if (can? mode) 'can-exploration (void))))]
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

  ;; unpause-to-trap : thread? (or/c trap+vals? exn?) -> void
  ;; unpause all of the paused children threads of `parent-thread`,
  ;; sending them to `trap-to-exit-to` instead
  (define (unpause-to-trap parent-thread trap-to-exit-to)
    (let loop ([parent-thread parent-thread]
               [first-one? #t])
      (match-define (par-state result/checkpoint-chan presence-waiting value-waiting paused active the-trap-of-this-par)
        (hash-ref parent->par-state parent-thread))
      (unless first-one?
        (when (or (exn? the-trap-of-this-par) (trap+vals? the-trap-of-this-par))
          (internal-error
           "found a par as we went down to unpause things that has a trap, par-parent: ~s"
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
                      (par-state result/checkpoint-chan
                                 presence-waiting value-waiting
                                 (set) (set-union paused active)
                                 the-trap-of-this-par)))))

  (let loop ()

    ;; this is a kind of abuse of the logging system; we skip these checks unless
    ;; someone is listening on the debugging logger
    (log-esterel-par-debug
     "checking invariants of par-parents / running-threads / parent->par-state~a"
     (begin
       (for ([(par-parent a-par-state) (in-hash parent->par-state)])
         (match-define (par-state checkpoint/result-chan presence-waiting value-waiting paused active a-trap)
           a-par-state)
         (for ([child-thread (in-set (set-union presence-waiting value-waiting
                                                paused active))])
           (unless (equal? (hash-ref par-parents child-thread #f) par-parent)
             (internal-error "parents relationship lost; parent ~s child ~s" par-parent child-thread)))

         (for ([active-child-thread (in-set active)])
           (unless (or (set-member? running-threads active-child-thread)
                       (hash-has-key? parent->par-state active-child-thread))
             (internal-error "par ~s has active child ~s that isn't in a parent nor in running-threads"
                             par-parent active-child-thread))))
       ""))

    (cond

      ;; the instant is over
      [(and (= 0 (hash-count presence-waiters))
            (= 0 (hash-count value-waiters))
            (set-empty? running-threads)
            instant-complete-chan)
       (log-esterel-debug "~a: ~a"
                          (eq-hash-code (current-thread))
                          (if (can? mode)
                              (format "finished a stage of can exploration: ~a; emits ~s; ~a more stages"
                                      (signal-states->hash (car (can-stages mode)))
                                      (can-emits mode)
                                      (length (cdr (can-stages mode))))
                              "instant has completed"))
       (log-esterel-info (if (can? mode)
                             (format "finishing a can stage; emits: ~a"
                                     (can-emits mode))
                             "finished instant"))
       (log-par-state)
       (cond
         [(can? mode)
          ;; we've finished the instant in can mode
          (define current-stage (car (can-stages mode)))
          (define atomic-considered-signals
            (set-union
             (can-stage-newly-ready current-stage)
             (can-considered-signals mode)
             (for/fold ([atomic-considered-signals (set)])
                       ([a-signal (in-list (can-stage-ordered-signals current-stage))])
               (define sp (compute-signal-presence a-signal signal-status))
               (cond
                 [(set? sp) (set-union sp atomic-considered-signals)]
                 [else atomic-considered-signals]))))
          (cond
            [(signal-states-done? current-stage)
             (cond
               [(null? (cdr (can-stages mode)))
                ;; we've explored all possibilities of relevant signals
                ;; either go back to must mode or report the discovery
                ;; of a non-constructive program
                (define unemitted-signals (set-subtract atomic-considered-signals (can-emits mode)))
                (cond
                  [(set-empty? unemitted-signals)
                   (channel-put instant-complete-chan
                                (cons 'non-constructive atomic-considered-signals))
                   ;; when we send back 'non-constructive, then we will
                   ;; never come back to this thread again, so just let it expire
                   (void)]
                  [else
                   (set! mode #f)
                   (restore-must-state)
                   (set! signal-status
                         (for/fold ([signal-status signal-status])
                                   ([a-signal (in-set unemitted-signals)])
                           (hash-set signal-status a-signal #f)))
                   (set! signal-ready (set-union
                                       signal-ready
                                       (for/set ([a-signal (in-set unemitted-signals)]
                                                 #:when (atomic-signal-combine a-signal))
                                         a-signal)))
                   (unblock-presence-threads #f)
                   (unblock-value-threads (set->list unemitted-signals))
                   (loop)])]
               [else
                ;; we've finished one stage but there are more stages to complete,
                ;; go back to work on the previous stage; this will land us right
                ;; back in the same case, but as if we've just finished the outer
                ;; stage, so we'll move on to the next one there (or do this again)
                (set! mode (struct-copy can mode
                                        [considered-signals atomic-considered-signals]
                                        [stages (cdr (can-stages mode))]))
                (loop)])]
            [else
             ;; we've got more possible signal values to explore; set them up
             ;; and go back to the rollback point to try them out
             (define new-stage (inc-signal-states current-stage))
             (set! mode (struct-copy can mode [stages (cons new-stage (cdr (can-stages mode)))]))
             (rollback! (can-stage-starting-point current-stage))
             (unblock-presence-threads (signal-states->hash new-stage))
             (unblock-value-threads (hash-keys value-waiters))
             (loop)])]
         [(pair? raised-exns)
          (channel-put instant-complete-chan (car raised-exns))
          ;; there was an exception while in must mode so just report that
          ;; and give up on this instant; currently throwing away
          ;; exceptions if there are multiples; not sure what to do about that
          (void)]
         [else
          ;; we finished the instant in must mode so
          ;; close the instant down and let `react!` know
          (channel-put instant-complete-chan
                       (hash-union
                        (for/hash ([(s v) (in-hash signal-status)]
                                   #:unless (atomic-signal-combine s)

                                   ;; drop suspended signals from the result
                                   #:when (boolean? v))
                          ;; avoid all valued signals from this part
                          (values s v))
                        signal-value))
          (set! instant-complete-chan #f)
          ; save the signals from previous instants
          (define (keep-count new old count)
            (let loop ([l (cons new old)]
                       [i count])
              (cond
                [(zero? i) '()]
                [(null? l) '()]
                [else (cons (car l) (loop (cdr l) (- i 1)))])))
          (set! signals-pre
                (keep-count
                 (for/set ([(a-signal val) (in-hash signal-status)]
                           #:when
                           (match val
                             [#t #t]
                             [#f #f]
                             ['suspended
                              (and (pair? signals-pre)
                                   (set-member? (car signals-pre)
                                                a-signal))]))
                   a-signal)
                 signals-pre
                 pre-count))
          (set! signal-values-pre (keep-count
                                   (if (pair? signal-values-pre)
                                       (carry-pre-forward signal-value (car signal-values-pre))
                                       signal-value)
                                   signal-values-pre
                                   ;; make sure we keep at least 1 for signal value so
                                   ;; we have a value for signals not emitted this instant
                                   (max 1 pre-count)))
          ;; reset the signals in preparation for the next instant
          (set! signal-status (hash))
          (set! signal-ready (set))
          (set! signal-value (hash))
          (set! raised-exns '()) ;; TODO: is this right?
          (set! instant-number (+ instant-number 1))
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
           (set!-values (signal-status signal-value)
                        (for/fold ([signal-status signal-status]
                                   [signal-value signal-value])
                                  ([signal-to-emit (in-list signals-to-emit)])
                          (match signal-to-emit
                            [(? atomic-signal?)
                             (values (hash-set signal-status signal-to-emit #t)
                                     signal-value)]
                            [(cons (? atomic-signal? signal-to-emit) val)
                             (values (hash-set signal-status signal-to-emit #t)
                                     (hash-set signal-value signal-to-emit val))])))
           (when first-instant-sema
             ;; this blocks starting the first instant; after that it isn't needed
             (semaphore-post first-instant-sema)
             (set! first-instant-sema #f))
           (set! instant-complete-chan _instant-complete-chan)
           (for ([(paused-thread resp-chan) (in-hash paused-threads)])
             (define thd-and-parent-thds
               (let loop ([thd paused-thread]
                          [thds (list paused-thread)])
                 (cond
                   [(hash-ref par-parents thd #f) => (λ (v) (loop v (cons v thds)))]
                   [else thds])))
             ;; parent threads come first in this list so
             ;; that the traversal of the suspends can go
             ;; from outermost to innermost
             (channel-put resp-chan thd-and-parent-thds)
             (add-running-thread paused-thread))
           (set! paused-threads (hash))
           (set! parent->par-state
                 (for/hash ([(parent-thread a-par-state) (in-hash parent->par-state)])
                   (match-define (par-state checkpoint/result-chan
                                            presence-waiting
                                            value-waiting
                                            paused
                                            active
                                            a-trap)
                     a-par-state)
                   ;; paused threads become active threads and
                   ;; there are no more paused threads when
                   ;; picking up after an instant has terminated
                   (values parent-thread (par-state checkpoint/result-chan
                                                    presence-waiting value-waiting
                                                    (set) paused
                                                    (if (esterel-thread-value? a-trap)
                                                        a-trap
                                                        (esterel-thread-value (set)))))))
           (loop))))]

      ;; nothing is running, but at least one thread is
      ;; waiting for a signal's value; switch into Can mode
      [(set-empty? running-threads)
       (log-esterel-debug "~a: ~a; ~s ~s ~s"
                          (eq-hash-code (current-thread))
                          (if (can? mode)
                              "entering new can stage"
                              "entering can mode")
                          (hash-keys presence-waiters)
                          (hash-keys value-waiters)
                          (and mode (can-emits mode)))
       (log-esterel-info "entering can mode: blocked on ~s"
                         (hash-keys presence-waiters))
       (when (and (= 0 (hash-count presence-waiters))
                  (= 0 (hash-count value-waiters)))
         (internal-error "expected some thread to be blocked on a signal"))
       (define ordered-signals
         (for/list ([(a-signal blocked-threads) (in-hash presence-waiters)])
           a-signal))
       (define newly-ready (list->set (hash-keys value-waiters)))
       (define new-can-stage (can-stage newly-ready
                                        (get-starting-point)
                                        ordered-signals
                                        0))
       (cond
         [(can? mode)
          ;; we're already in can mode and blocked so start a new stage
          (set! mode
                (struct-copy can mode [stages (cons new-can-stage (can-stages mode))]))]
         [else
          ;; otherwise, we need to start up a new can mode
          (save-must-state)
          (set! mode (can (set)
                          (set)
                          (list new-can-stage)))])
       (rollback! (can-stage-starting-point new-can-stage))
       (unblock-presence-threads (signal-states->hash new-can-stage))
       (unblock-value-threads (hash-keys value-waiters))
       (loop)]

      ;; an instant is running, handle the various things that can happen during it
      [else
       (sync
        (handle-evt
         new-signal-chan
         (λ (chan)
           (channel-put chan new-signal-counter)
           (set! new-signal-counter (+ new-signal-counter 1))
           (loop)))
        (handle-evt
         signal-presence/value-chan
         (λ (isp+s+thd+resp+pre)
           (match-define (vector is-present? a-signal the-thread resp-chan pre) isp+s+thd+resp+pre)
           (log-esterel-debug (if is-present?
                                  (format "~a: testing signal presence ~s ~s ~s"
                                          (eq-hash-code (current-thread))
                                          a-signal
                                          (hash-ref signal-status a-signal 'unknown)
                                          the-thread)
                                  (format "~a: getting a signal's value ~s ~s"
                                          (eq-hash-code (current-thread))
                                          a-signal
                                          the-thread)))
           (log-par-state)
           (cond
             [(= pre 0)
              (define a-signals-presence (and is-present? (compute-signal-presence a-signal signal-status)))
              (cond
                [(equal? 'suspended (hash-ref signal-status a-signal #f))
                 (channel-put resp-chan (signal-suspended))]
                [(if is-present?
                     (set? a-signals-presence)
                     (not (set-member? signal-ready a-signal)))
                 ;; here we need to block
                 (remove-running-thread the-thread)
                 (if is-present?
                     (add-presence-waiter! a-signal the-thread resp-chan)
                     (add-value-waiter! a-signal the-thread resp-chan))
                 (define parent-thread (hash-ref par-parents the-thread #f))
                 (when parent-thread
                   (define old-par-state (hash-ref parent->par-state parent-thread))
                   (define new-par-state
                     (struct-copy
                      par-state old-par-state
                      [active (set-remove (par-state-active old-par-state) the-thread)]
                      [presence-waiting (if is-present?
                                            (set-add (par-state-presence-waiting old-par-state)
                                                     the-thread)
                                            (par-state-presence-waiting old-par-state))]
                      [value-waiting (if is-present?
                                         (par-state-value-waiting old-par-state)
                                         (set-add (par-state-value-waiting old-par-state)
                                                  the-thread))]))
                   (set! parent->par-state (hash-set parent->par-state parent-thread new-par-state)))]
                [else
                 (define response
                   (if is-present?
                       a-signals-presence
                       (get-signals-value a-signal)))
                 (channel-put resp-chan response)])]
             [else
              (let loop ([loop-pre (- pre 1)]
                         [loop-signals-pre (if is-present? signals-pre signal-values-pre)])
                (cond
                  [(empty? loop-signals-pre)
                   (channel-put resp-chan (if is-present?
                                              #f
                                              (signal-never-before-emitted)))]
                  [(zero? loop-pre)
                   (channel-put resp-chan
                                (if is-present?
                                    (set-member? (car loop-signals-pre) a-signal)
                                    (hash-ref (car loop-signals-pre)
                                              a-signal
                                              signal-never-before-emitted)))]
                  [else (loop (- loop-pre 1) (cdr loop-signals-pre))]))])
           (loop)))
        (handle-evt
         signal-dead-chan
         (λ (signal-or-signals)
           ;; don't try to update `signal-status` here; this guarantees
           ;; no future emits so we'll get `signal-status` updated properly later
           (cond
             [(set? signal-or-signals)
              (set! dead-signals (set-union dead-signals signal-or-signals))]
             [else
              (set! dead-signals (set-add dead-signals signal-or-signals))])
           (loop)))
        (handle-evt
         emit-chan
         (λ (a-signal+value-provided+value+resp-chan)
           (match-define (vector a-signal no-value-provided? a-value resp-chan)
             a-signal+value-provided+value+resp-chan)
           (define value-provided? (not no-value-provided?))
           (log-esterel-debug "~a: emitting: ~s status: ~s value: ~a"
                              (eq-hash-code (current-thread))
                              a-signal
                              (hash-ref signal-status a-signal 'unknown)
                              (if value-provided?
                                  (format "~.s" a-value)
                                  "<< no value provided >>"))
           (log-par-state)
           (cond
             [(set-member? dead-signals a-signal)
              ;; if the signal is outside its scope, then only send
              ;; back the message to trigger an error
              (channel-put resp-chan 'dead)
              (loop)]
             [else
              (when (can? mode)
                ;; we're in can mode, so record that this signal can be emitted
                (set! mode (add-emitted-signal mode a-signal)))
              (let/ec done
                (when value-provided?
                  (unless (can? mode)
                    ;; don't update the signal's value in can mode for fear
                    ;; of race conditions; when the emission happens, we'll
                    ;; definitely not set this signal to be ready, anyway.
                    (define new-value
                      (cond
                        [(hash-has-key? signal-value a-signal)
                         (define comb (atomic-signal-combine a-signal))
                         (define prev (hash-ref signal-value a-signal))
                         (when (equal? comb 'single)
                           (channel-put resp-chan (signal-single-emitted-twice prev))
                           (done))
                         (comb prev a-value)]
                        [else a-value]))
                    (set! signal-value (hash-set signal-value a-signal new-value))
                    (when (set-member? signal-ready a-signal)
                      (channel-put resp-chan (signal-ready-and-emitted a-value))
                      (done))))
                (match (hash-ref signal-status a-signal 'unknown)
                  ['unknown
                   ;; the signal's not dead, so send back a message that doesn't trigger
                   ;; an error and continue, updating the instant state for the emit
                   (channel-put resp-chan (void))
                   (unless (can? mode)
                     ;; when in can mode we should ignore emission so we
                     ;; don't cut off branches that have potential emitters
                     (set! signal-status (hash-set signal-status a-signal #t))
                     (define newly-unblocked (update-presence-waiters-following-emission))
                     (for ([(some-blocked-threads val) (in-hash newly-unblocked)])
                       (for ([a-blocked-thread (in-list some-blocked-threads)])
                         (match-define (blocked-thread thread resp-chan) a-blocked-thread)
                         (channel-put resp-chan val)
                         (define parent-thread (hash-ref par-parents thread #f))
                         (when parent-thread
                           (define old-par-state (hash-ref parent->par-state parent-thread))
                           (define new-par-state
                             (struct-copy
                              par-state old-par-state
                              [presence-waiting (if value-provided?
                                                    (par-state-presence-waiting old-par-state)
                                                    (set-remove (par-state-presence-waiting old-par-state) thread))]
                              [value-waiting (if value-provided?
                                                 (set-remove (par-state-value-waiting old-par-state) thread)
                                                 (par-state-value-waiting old-par-state))]
                              [active (set-add (par-state-active old-par-state) thread)]))
                           (set! parent->par-state (hash-set parent->par-state parent-thread new-par-state)))
                         (add-running-thread thread))))]
                  ['suspended
                   ;; the signal's suspended, so send back a message that triggers an error
                   (channel-put resp-chan (signal-suspended))]

                  [#t
                   ;; the signal's not dead, so send back a message that doesn't trigger
                   ;; an error and continue, updating the instant state for the emit
                   (channel-put resp-chan (void))
                   ;; the signal has been emitted before; nothing else to do
                   ]
                  [#f
                   ;; the signal's not dead, so send back a message that doesn't trigger
                   ;; an error and continue, updating the instant state for the emit
                   (channel-put resp-chan (void))

                   (unless (can? mode)
                     ;; if the signal isn't present but it got emitted (and we aren't in can mode)
                     ;; then something has gone wrong; let's crash.
                     (internal-error "emission of an absent signal\n  signal: ~s"
                                     a-signal))]))
              (loop)])))
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
           (set! parent->par-state
                 (hash-set parent->par-state parent-thread
                           (par-state checkpoint/result-chan (set) (set) (set) children-threads
                                      (esterel-thread-value (set)))))
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
         suspended-signals-chan
         (λ (signals+resp-chan)
           (match-define (cons signals resp-chan) signals+resp-chan)
           (define resp #f)
           (for ([a-signal (in-set signals)])
             (when (and (hash-has-key? signal-status a-signal)
                        (boolean? (hash-ref signal-status a-signal)))
               (set! resp a-signal))
             (set! signal-status (hash-set signal-status a-signal 'suspended)))
           (channel-put resp-chan resp)
           (loop)))
        (handle-evt
         react-thread-done-chan
         (λ (exn)
           (log-esterel-debug "~a: main thread done, ~s" (eq-hash-code (current-thread)) exn)
           (log-par-state)
           (when (exn? exn) (set! raised-exns (cons exn raised-exns)))
           (remove-running-thread esterel-thread)
           (loop)))
        (handle-evt
         when-must-chan
         (λ (resp)
           (channel-put resp (not (can? mode)))
           (loop)))
        (handle-evt
         instant-number-chan
         (λ (resp-chan)
           (channel-put resp-chan instant-number)
           (loop)))
        )])))

(struct kernel-esterel.rkt::internal-error exn:fail ())
(define (internal-error fmt . args)
  (raise
  (kernel-esterel.rkt::internal-error
   (string-append "kernel-esterel.rkt: internal error: " (apply format fmt args))
   (current-continuation-marks))))

(define (carry-pre-forward signal-value signal-values-pre)
  (for/hash ([k (in-set (set-union (list->set (hash-keys signal-value))
                                     (list->set (hash-keys signal-values-pre))))]
               #:unless (and (memoryless-signal? k)
                             (not (hash-has-key? signal-value k))))
    (values k (hash-ref signal-value k (λ () (hash-ref signal-values-pre k))))))
