#lang racket
(require "structs.rkt" "search-state.rkt"
         (for-syntax syntax/parse))

(provide
 (rename-out [-reaction reaction])
 (rename-out [-signal signal])
 par
 suspend
 with-trap exit-trap
 (contract-out
  [react! (-> reaction? (hash/c signal? boolean? #:immutable #t #:flat? #t))]
  [in-reaction? (-> boolean?)]
  [signal-value (->* (signal?)
                     #:pre (in-reaction?)
                     boolean?)]
  [signal? (-> any/c boolean?)]
  [emit (->* (signal?)
             #:pre (in-reaction?)
             void?)]
  [pause (->* () #:pre (in-reaction?) void?)]))

(define-logger esterel)

(struct checkpoint-request (resp-chan))
(define reaction-prompt-tag (make-continuation-prompt-tag 'reaction))

(define current-signal-table (make-parameter #f))
(define (in-reaction?) (and (current-signal-table) #t))

(define-syntax (-signal stx)
  (syntax-case stx ()
    [(_) #`(mk-signal.1 '#,(syntax-local-name))]
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
  (define parent-thread (current-thread))
  (define result-chans+children-threads
    (for/set ([thunk (in-list thunks)])
      ;; result-chan : channel[(or/c #f trap?)]
      (define par-child-result-chan (make-channel))
      (define par-child-thread
        (make-rebuilt-thread
         #:parent-thread parent-thread
         #:before-par-trap-counter before-par-trap-counter
         #:par-child-result-chan par-child-result-chan
         thunk))
      (cons par-child-result-chan par-child-thread)))
  (define signal-table (current-signal-table))
  (define checkpoint-chan (make-channel))
  (channel-put (signal-table-par-start-chan signal-table)
               (vector checkpoint-chan
                       (current-thread)
                       result-chans+children-threads))
  (for ([_ (in-list thunks)]) (semaphore-post s))
  (let loop ([pending-result-chans+par-threads result-chans+children-threads]
             [checkpoint-chan checkpoint-chan]
             [current-trap #f])
    (cond
      [(set-empty? pending-result-chans+par-threads)
       (when current-trap (exit-trap current-trap))]
      [else
       (apply
        sync
        (handle-evt
         checkpoint-chan
         (λ (resp-chan)
           (define-values (new-pending-result-chans+par-threads new-checkpoint-chan new-trap)
             (call/cc
              (λ (k)
                (channel-put resp-chan (vector current-trap before-par-trap-counter k))
                (values pending-result-chans+par-threads checkpoint-chan current-trap))
              reaction-prompt-tag))
           (loop new-pending-result-chans+par-threads new-checkpoint-chan new-trap)))
        (for/list ([result-chan+pending-par-thread (in-set pending-result-chans+par-threads)])
          (match-define (cons result-chan pending-par-thread) result-chan+pending-par-thread)
          (handle-evt
           result-chan
           (λ (new-trap)
             (channel-put (signal-table-par-partly-done-chan signal-table)
                          (vector (current-thread) pending-par-thread new-trap))
             (loop (set-remove pending-result-chans+par-threads result-chan+pending-par-thread)
                   checkpoint-chan
                   (outermost-trap new-trap current-trap))))))])))

;; we didn't save the parameterization at the point of the pause/signal-value/par
;; and so we cannot restore it here; not sure if this is important or not, tho!
(define (make-rebuilt-thread #:parent-thread parent-thread
                             #:before-par-trap-counter before-par-trap-counter
                             #:par-child-result-chan par-child-result-chan
                             thunk)
  (define thunk-to-run
    (cond
      [before-par-trap-counter
       (λ ()
         (channel-put
          par-child-result-chan
          (let/ec escape
            (with-continuation-mark trap-start-of-par-mark
              (vector parent-thread escape before-par-trap-counter)
              (with-continuation-mark trap-counter-mark before-par-trap-counter
                (begin
                  (call-with-continuation-prompt
                   thunk
                   reaction-prompt-tag)
                  #f))))))]
      [else
       (λ ()
         (call-with-continuation-prompt
          thunk
          reaction-prompt-tag))]))
  ;; this renaming doesn't work when rebuilding the threads currently
  (thread (procedure-rename thunk-to-run (object-name thunk))))

;; outermost-trap : (or/c #f trap?) (or/c #f trap?) -> (or/c #f trap?)
(define (outermost-trap t1 t2)
  (cond
    [(and (trap? t1) (trap? t2))
     (if (< (trap-counter t1) (trap-counter t2))
         t1
         t2)]
    [else (or t1 t2)]))

(define (pause)
  (define signal-table (current-signal-table))
  (define resp-chan (make-channel))
  (define par-info (continuation-mark-set-first #f trap-start-of-par-mark))
  (define parent-thread (and par-info (vector-ref par-info 0)))
  (channel-put (signal-table-pause-chan signal-table)
               (vector parent-thread (current-thread) resp-chan))
  (let loop ([resp-chan resp-chan])
    (define val (channel-get resp-chan))
    (cond
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
           (when (signal-value (vector-ref next-val 0)) (pause))
           (loop next-iter)))
       val])))

(define-syntax (suspend stx)
  (syntax-case stx ()
    [(_ e s)
     #'(suspend/proc (λ () e) s)]))

(define suspend-mark (gensym 'suspend))
(define (suspend/proc body signal)
  (unless (in-reaction?) (error 'suspend "not in a reaction"))
  (with-continuation-mark suspend-mark signal
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
(define (get-start-of-par-counter) (continuation-mark-set-first #f trap-start-of-par-mark))
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

(define (exit-trap trap)
  (define start-of-par-counter (get-start-of-par-counter))
  (cond
    [(or (not start-of-par-counter)
         ;; the start of the par value will be the same as
         ;; the counter used for the first trap inside the par
         ;; so this should be an inclusive comparison
         (<= (vector-ref start-of-par-counter 2) (trap-counter trap)))
     ;; here the trap doesn't span a par, so we can just escape
     ((trap-escape trap) (void))]
    [else
     ;; here we tell the enclosing par that a trap has happened
     ((vector-ref start-of-par-counter 1) trap)]))

(struct signal-table (signal-chan
                      emit-chan
                      par-start-chan par-partly-done-chan
                      pause-chan instant-chan
                      react-thread-done-chan))
  
(struct reaction (signal-table))
(define-syntax-rule (-reaction e1 e2 ...) (reaction/proc (λ () e1 e2 ...)))
(define (reaction/proc thunk)
  (define the-signal-table
    (signal-table (make-channel) (make-channel) (make-channel)
                  (make-channel) (make-channel) (make-channel) (make-channel)))
  (thread (λ () (run-reaction-thread thunk the-signal-table)))
  (reaction the-signal-table))

(define (react! a-reaction)
  (define signal-table (reaction-signal-table a-reaction))
  (define instant-complete-chan (make-channel))
  (channel-put (signal-table-instant-chan signal-table) instant-complete-chan)
  (define maybe-signals (channel-get instant-complete-chan))
  (cond
    [(equal? maybe-signals 'non-constructive)
     (error 'react! "the program is not constructive")]
    [maybe-signals
     ;; copy the hash to an immutable one to hand out
     (for/hash ([(k v) (in-hash maybe-signals)])
       (values k v))]
    [else
     (error 'react! "a reaction is already running")]))

(define (run-reaction-thread thunk the-signal-table)
  (define first-instant-sema (make-semaphore 0))
  (define reaction-thread
    (let ([first-instant-sema first-instant-sema])
      (parameterize ([current-signal-table the-signal-table])
        (define (reaction-thread-thunk)
          (semaphore-wait first-instant-sema)
          (call-with-continuation-prompt
           (λ ()
             (thunk)
             (channel-put (signal-table-react-thread-done-chan the-signal-table) (void)))
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
  (define/contract paused-threads
    (hash/c thread? channel? #:flat? #t #:immutable #t)
    (hash))

  ;; (set/c hash)
  ;; all of the threads in the reaction that aren't
  ;;    blocked on a signal and have not paused
  ;;    and are not a par-parent thread
  (define running-threads (set reaction-thread))
  (define/contract (add-running-thread t)
    (-> thread? void?)
    (set! running-threads (set-add running-threads t)))
  (define/contract (add-running-threads ts)
    (-> (set/c thread?) void?)
    (set! running-threads (set-union running-threads ts)))
  (define (remove-running-thread t)
    (set! running-threads (set-remove running-threads t)))

  ;; a parent thread (of a par) points to a set of its children
  (define/contract par-children
    (hash/c thread? (set/c thread?) #:flat? #t #:immutable #t)
    (hash))

  ;; each parent thread (of a par) points to the channel
  ;;    that it listens for checkpoint requests on
  ;; the domain of this map is the set of threads that
  ;;    are currently parents of a `par`
  (define/contract par-checkpoint-chans
    (hash/c thread? channel? #:flat? #t #:immutable #t)
    (hash))

  ;; each parent thread (of a par) points to the outermost
  ;;    trap that any of its children exited to; if
  ;;    unmapped none of the children have exited to a trap
  (define/contract par-trap
    (hash/c thread? trap? #:flat? #t #:immutable #t)
    (hash))

  ;; (or/c #f (chan/c (or/c (hash/c signal? boolean?) #f)))
  ;; #f means we're not in an instant, chan means we are.
  ;; #f is sent back is a message to signal an error
  ;; in `react!` and the hash are the signal values that
  ;; got taken in this instant. Any chan that gets put into
  ;; never gets, #f, tho. Only those that don't get put here do
  (define instant-complete-chan #f)


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
        [wrong-guess?
         (set! wrong-guess? #f)
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
      (define blocked-threads (hash-ref signal-waiters signal-to-be-absent))
      (set! signals (hash-set signals signal-to-be-absent #f))
      (set! signal-waiters (hash-remove signal-waiters signal-to-be-absent))
      (for ([a-blocked-thread (in-list blocked-threads)])
        (match-define (blocked-thread thread resp-chan) a-blocked-thread)
        (channel-put resp-chan #f)
        (add-running-thread thread))))

  ;; rb-tree : rb-tree?
  ;; signals : (hash/c signal? boolean? #:flat? #t #:immutable #t)
  (struct rollback-point (rb-tree signals))
  (define (rollback! a-rollback-point)
    (match-define (rollback-point rb-tree rb-signals) a-rollback-point)
    (set! signals rb-signals)
    (set! signal-waiters (hash))
    (set! paused-threads (hash))
    (set! par-children (hash))
    (set! par-checkpoint-chans (hash))
    (set! reaction-thread (rebuild-threads-from-rb-tree rb-tree)))
  (define (current-rollback-point) (rollback-point (build-rb-tree-from-current-state) signals))

  (struct rb-tree ())

  ;; cont : continuation[listof thread]
  ;; children : (set/c rb?)
  (struct rb-par rb-tree (cont children current-trap before-par-trap-counter))

  ;; cont : continuation[channel]
  (struct rb-paused rb-tree (cont))

  ;; cont : continuation[channel]
  (struct rb-blocked rb-tree (signal cont))

  ;; build-rb-tree-from-current-state : -> rb-tree?
  (define (build-rb-tree-from-current-state)
    (define k-chan (make-channel))
    (define a-checkpoint-request (checkpoint-request k-chan))
    (let loop ([thread reaction-thread])
      (cond
        [(hash-has-key? par-children thread)
         (define checkpoint-chan (hash-ref par-checkpoint-chans thread))
         (channel-put checkpoint-chan k-chan)
         ;; these continuations accept a list of threads to wait for and
         ;; continue the loop in `par` with them; to implement `trap`,
         ;; we probably need to communicate more information back and forth here
         ;; as `par` needs to do end pauses when traps happen
         (match-define (vector current-trap before-par-trap-counter par-continuation)
           (channel-get k-chan))
         (rb-par
          par-continuation
          (for/set ([child (in-set (hash-ref par-children thread))])
            (define chan (make-channel))
            (loop child))
          current-trap
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
        [else (error 'collect-rollback-points "lost a thread ~s" thread)])))

  ;; rebuild-threads-from-rb-tree : rb-tree? -> thread
  ;; returns the new reaction thread
  (define (rebuild-threads-from-rb-tree rb-tree)
    (let loop ([rb-tree rb-tree]
               [parent-thread #f]
               [par-child-result-chan #f]
               [parent-before-par-trap-counter #f])
      (match rb-tree
        [(rb-par par-cont rb-children current-trap before-par-trap-counter)
         ;; we have to do this strange dance with `sema` in order to make
         ;; the recursive call to `loop`, as we need the par parent
         ;; thread to make the call. So we avoid race
         ;; conditions by blocking on `sema` until we've completed the
         ;; recursive calls and updated the instant state
         (define sema (make-semaphore))
         (define par-parent-thread
           (parameterize ([current-signal-table the-signal-table])
             (make-rebuilt-thread
              #:parent-thread parent-thread
              #:before-par-trap-counter parent-before-par-trap-counter
              #:par-child-result-chan par-child-result-chan
              (λ ()
                (define par-thread (current-thread))
                (define result-chans+children-threads
                  (for/set ([child (in-set rb-children)])
                    (define par-child-result-chan (make-channel))
                    (cons par-child-result-chan
                          (loop child
                                par-thread
                                par-child-result-chan
                                before-par-trap-counter))))
                (define this-par-children
                  (for/set ([result-chan+children-thread (in-set result-chans+children-threads)])
                    (cdr result-chan+children-thread)))
                (define checkpoint-chan (make-channel))
                (set! par-children (hash-set par-children par-thread this-par-children))
                (set! par-checkpoint-chans (hash-set par-checkpoint-chans par-thread checkpoint-chan))
                (semaphore-post sema)
                (par-cont result-chans+children-threads checkpoint-chan current-trap)))))
         (semaphore-wait sema)
         par-parent-thread]
        [(rb-paused cont)
         (define pause-chan (make-channel))
         (define paused-thread
           (parameterize ([current-signal-table the-signal-table])
             (make-rebuilt-thread
              #:parent-thread parent-thread
              #:before-par-trap-counter parent-before-par-trap-counter
              #:par-child-result-chan par-child-result-chan
              (λ () (cont pause-chan)))))
         (set! paused-threads (hash-set paused-threads paused-thread pause-chan))
         paused-thread]
        [(rb-blocked a-signal cont)
         (define resp-chan (make-channel))
         (define blocked-thread
           (parameterize ([current-signal-table the-signal-table])
             (make-rebuilt-thread
              #:parent-thread parent-thread
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

  
  (let loop ()
    (cond
      [non-constructive-program?
       (log-esterel-debug "~a: non constructive program" (eq-hash-code (current-thread)))
       (channel-put instant-complete-chan 'non-constructive)
       (loop)]

      ;; the instant is over
      [(and (= 0 (hash-count signal-waiters))
            (set-empty? running-threads)
            instant-complete-chan)
       (log-esterel-debug "~a: instant is over: ~s" (eq-hash-code (current-thread)) wrong-guess?)
       (cond
         [wrong-guess?
          ;; although we got to the end of the instant, we did so with
          ;; a wrong guess for signal absence; go back and try again
          (choose-a-signal-to-be-absent)
          (loop)]
         [else
          ;; close the instant down and let `react!` know
          (channel-put instant-complete-chan signals)
          (set! instant-complete-chan #f)
          (set! signals (hash)) ;; reset the signals in preparation for the next instant
          (loop)])]

      ;; an instant is not runnning, wait for one to start (but don't wait for other stuff)
      [(not instant-complete-chan)
       (log-esterel-debug "~a: waiting for an instant to start" (eq-hash-code (current-thread)))
       (sync
        (handle-evt
         instant-chan
         (λ (_instant-complete-chan)
           (when first-instant-sema
             ;; this blocks starting the first instant; after that it isn't needed
             (semaphore-post first-instant-sema)
             (set! first-instant-sema #f))
           (set! instant-complete-chan _instant-complete-chan)
           (for ([(paused-thread resp-chan) (in-hash paused-threads)])
             (channel-put resp-chan (void))
             (add-running-thread paused-thread))
           (set! paused-threads (hash))
           (loop))))]

      ;; everyone's blocked on signals or paused; pick a signal to set to be absent
      [(set-empty? running-threads)
       (when (= 0 (hash-count signal-waiters))
         (error 'esterel.rkt "internal error; expected someone to be blocked on a signal"))
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
                              (eq-hash-code a-signal)
                              (hash-ref signals a-signal 'unknown)
                              the-thread)
           (match (hash-ref signals a-signal 'unknown)
             ['unknown
              (remove-running-thread the-thread)
              (add-signal-waiter! a-signal the-thread resp-chan)]
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
                              (eq-hash-code a-signal)
                              (hash-ref signals a-signal 'unknown))
           (match (hash-ref signals a-signal 'unknown)
             ['unknown
              (set! signals (hash-set signals a-signal #t))
              (for ([a-blocked-thread (hash-ref signal-waiters a-signal '())])
                (match-define (blocked-thread thread resp-chan) a-blocked-thread)
                (channel-put resp-chan #t)
                (add-running-thread thread))
              (set! signal-waiters (hash-remove signal-waiters a-signal))]
             [#t
              (void)]
             [#f
              (set! wrong-guess? #t)])
           (loop)))
        (handle-evt
         par-start-chan
         (λ (checkpoint-chan+parent+children-threads)
           (match-define (vector checkpoint-chan parent-thread result-chans+children-threads)
             checkpoint-chan+parent+children-threads)
           (define children-threads (for/set ([x (in-set result-chans+children-threads)]) (cdr x)))
           (log-esterel-debug "~a: starting a par ~s ~s"
                              (eq-hash-code (current-thread))
                              parent-thread children-threads)
           (add-running-threads children-threads)
           (remove-running-thread parent-thread)
           (set! par-children (hash-set par-children parent-thread children-threads))
           (set! par-checkpoint-chans (hash-set par-checkpoint-chans parent-thread checkpoint-chan))
           (loop)))
        (handle-evt
         par-partly-done-chan
         (λ (parent-thread+done-thread+trap)
           (match-define (vector parent-thread done-thread new-trap) parent-thread+done-thread+trap)
           (log-esterel-debug "~a: one thread in a par finished ~s trap ~s"
                              (eq-hash-code (current-thread))
                              done-thread
                              new-trap)
           (remove-running-thread done-thread)
           (define children-to-remove (set done-thread))
           (define current-trap (hash-ref par-trap parent-thread #f))

           (define parents-new-children
             (set-remove (hash-ref par-children parent-thread) done-thread))

           (when (and (not current-trap) new-trap)
             ;; abort paused sibling threads if this is the first
             ;;    we've learned about a trap in this par
             ;; we abort them by telling the pause to wake up and
             ;;   become a trap; we need to restore the instant
             ;;   loop's state to do so (running threads and par children)
             (for ([child-thread (in-set (hash-ref par-children parent-thread))])
               (define resp-chan (hash-ref paused-threads child-thread #f))
               (when resp-chan
                 (channel-put resp-chan new-trap)
                 (set! parents-new-children (set-add parents-new-children child-thread))
                 (add-running-thread child-thread)
                 (set! paused-threads (hash-remove paused-threads child-thread)))))

           (when new-trap
             (set! par-trap
                   (hash-set par-trap parent-thread (outermost-trap current-trap new-trap))))
           (cond
             [(set-empty? parents-new-children)
              (set! par-children (hash-remove par-children parent-thread))
              (set! par-checkpoint-chans (hash-remove par-checkpoint-chans parent-thread))
              (add-running-thread parent-thread)]
             [else
              (set! par-children (hash-set par-children parent-thread parents-new-children))])
           (loop)))
        (handle-evt
         pause-chan
         (λ (parent-thread+thread+resp-chan)
           (match-define (vector parent-thread paused-thread resp-chan) parent-thread+thread+resp-chan)
           (log-esterel-debug "~a: paused ~s" (eq-hash-code (current-thread)) paused-thread)
           (cond
             [(and parent-thread (hash-ref par-trap parent-thread #f))
              =>
              (λ (a-trap)
                (channel-put resp-chan a-trap))]
             [else
              (remove-running-thread paused-thread)
              (set! paused-threads (hash-set paused-threads paused-thread resp-chan))])
           (loop)))
        (handle-evt
         instant-chan
         (λ (_instant-complete-chan)
           ;; someone tried to start an instant while one is running
           ;; let them know there's an error
           (channel-put _instant-complete-chan #f)
           (loop)))
        (handle-evt
         react-thread-done-chan
         (λ (_)
           (log-esterel-debug "~a: main reaction thread done" (eq-hash-code (current-thread)))
           (remove-running-thread reaction-thread)
           (loop)))
        )])))

