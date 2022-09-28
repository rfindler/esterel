#lang racket

(provide
 (rename-out [-reaction reaction])
 par
 (contract-out
  [react! (-> reaction? (hash/c signal? boolean? #:immutable #t #:flat? #t))]
  [in-reaction? (-> boolean?)]
  [signal-value (->* (signal?)
                     #:pre (in-reaction?)
                     boolean?)]
  [signal (-> signal?)]
  [signal? (-> any/c boolean?)]
  [emit (->* (signal?)
             #:pre (in-reaction?)
             void?)]
  [pause (->* () #:pre (in-reaction?) void?)]))

(struct signal ())
  
(define current-signal-table (make-parameter #f))
(define (in-reaction?) (and (current-signal-table) #t))
  
(define (emit a-signal)
  (define signal-table (current-signal-table))
  (channel-put (signal-table-emit-chan signal-table) a-signal))
  
(define (signal-value a-signal)
   (define signal-table (current-signal-table))
  (define resp-chan (make-channel))
  (channel-put (signal-table-signal-chan signal-table)
               (vector a-signal (current-thread) resp-chan))
  (channel-get resp-chan))

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
  (define children-threads
    (for/set ([thunk (in-list thunks)])
      (thread thunk)))
  (define signal-table (current-signal-table))
  (channel-put (signal-table-par-start-chan signal-table)
               (cons (current-thread) children-threads))
  (let loop ([pending-par-threads children-threads])
    (cond
      [(set-empty? pending-par-threads) (void)]
      [else
       (apply
        sync
        (for/list ([pending-par-thread (in-set pending-par-threads)])
          (handle-evt
           pending-par-thread
           (λ (_)
             (channel-put (signal-table-par-partly-done-chan signal-table)
                          (cons (current-thread) pending-par-thread))
             (loop (set-remove pending-par-threads pending-par-thread))))))])))

(define (pause)
  (define signal-table (current-signal-table))
  (define resp-chan (make-channel))
  (channel-put (signal-table-pause-chan signal-table)
               (cons (current-thread) resp-chan))
  (channel-get resp-chan))


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
  (define first-instant-sema (make-semaphore 0))
  (define (reaction-thread-thunk)
    (semaphore-wait first-instant-sema)
    (thunk)
    (channel-put (signal-table-react-thread-done-chan the-signal-table) (void)))
  (define reaction-thread
    (parameterize ([current-signal-table the-signal-table])
      (thread reaction-thread-thunk)))
  (thread (λ () (run-reaction-thread reaction-thread first-instant-sema the-signal-table)))
  (reaction the-signal-table))

(define (react! a-reaction)
  (define signal-table (reaction-signal-table a-reaction))
  (define instant-complete-chan (make-channel))
  (channel-put (signal-table-instant-chan signal-table) instant-complete-chan)
  (define maybe-signals (channel-get instant-complete-chan))
  (cond
    [maybe-signals
     ;; copy the hash to an immutable one to hand out
     (for/hash ([(k v) (in-hash maybe-signals)])
       (values k v))]
    [else
     (error 'react! "a reaction is already running")]))
     
(define (run-reaction-thread reaction-thread first-instant-sema the-signal-table)
  (match-define (signal-table signal-chan emit-chan
                              par-start-chan par-partly-done-chan
                              pause-chan instant-chan
                              react-thread-done-chan)
    the-signal-table)

  ;; hash[signal -o-> boolean?] -- if a signal isn't mapped, its value isn't yet known
  (define signals (make-hash))

  (struct blocked-thread (thread resp-chan) #:transparent)
    
  ;; hash[signal -o-> (listof blocked-thread?)]
  ;; threads that are blocked, waiting for a signal's value to be decided
  ;; (a missing entry is the same as the empty list)
  (define/contract signal-waiters
    (hash/c signal? (listof blocked-thread?))
    (make-hash))

  ;; hash[thread -> chan]
  ;; each paused thread is blocked on the corresponding channel
  (define/contract paused-threads
    (hash/c thread? channel?)
    (make-hash))

  ;; (set/c hash)
  ;; all of the threads in the reaction that aren't
  ;; blocked on a signal and have not paused
  (define running-threads (set reaction-thread))
  (define/contract (add-running-thread t)
    (-> thread? void?)
    (set! running-threads (set-add running-threads t)))
  (define/contract (add-running-threads ts)
    (-> (set/c thread?) void?)
    (set! running-threads (set-union running-threads ts)))
  (define (remove-running-thread t)
    (set! running-threads (set-remove running-threads t)))

  ;; hash[thread --> (setof thread)]
  ;; a parent points to a set of its children
  (define/contract par-children
    (hash/c thread? (set/c thread?))
    (make-hash))

  ;; (or/c #f (chan/c (or/c (hash/c signal? boolean?) #f)))
  ;; #f means we're not in an instant, chan means we are.
  ;; #f is sent back is a message to signal an error
  ;; in `react!` and the hash are the signal values that
  ;; got taken in this instant. Any chan that gets put into
  ;; never gets, #f, tho. Only those that don't get put here do
  (define instant-complete-chan #f)
  
  (define (choose-a-signal-to-be-absent)
    (define signal-to-be-absent (for/first ([(k v) (in-hash signal-waiters)]) k))
    (define blocked-threads (hash-ref signal-waiters signal-to-be-absent))
    (hash-set! signals signal-to-be-absent #f)
    (hash-remove! signal-waiters signal-to-be-absent)
    (for ([a-blocked-thread (in-list blocked-threads)])
      (match-define (blocked-thread thread resp-chan) a-blocked-thread)
      (channel-put resp-chan #f)
      (add-running-thread thread)))

  (let loop ()
    (cond
      ;; the instant is over, close it down and let `react!` know
      [(and (= 0 (hash-count signal-waiters))
            (set-empty? running-threads)
            instant-complete-chan)
       (channel-put instant-complete-chan signals)
       (set! instant-complete-chan #f)
       (set! signals (make-hash)) ;; reset the signals in preparation for the next instant
       (loop)]

      ;; an instant is not runnning, wait for one to start (but don't wait for other stuff)
      [(not instant-complete-chan)
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
           (set! paused-threads (make-hash))
           (loop))))]

      ;; an instant is running, handle the various things that can happen during it
      [else
       (sync
        (handle-evt
         signal-chan
         (λ (s+resp)
           (match-define (vector a-signal the-thread resp-chan) s+resp)
           (match (hash-ref signals a-signal 'unknown)
             ['unknown
              (remove-running-thread the-thread)
              (define a-blocked-thread (blocked-thread the-thread resp-chan))
              (hash-set! signal-waiters a-signal (cons a-blocked-thread (hash-ref signal-waiters a-signal '())))
              (when (set-empty? running-threads) (choose-a-signal-to-be-absent))]
             [#f
              (channel-put resp-chan #f)]
             [#t
              (channel-put resp-chan #t)])
           (loop)))
        (handle-evt
         emit-chan
         (λ (a-signal)
           (match (hash-ref signals a-signal 'unknown)
             ['unknown
              (hash-set! signals a-signal #t)
              (for ([a-blocked-thread (hash-ref signal-waiters a-signal '())])
                (match-define (blocked-thread thread resp-chan) a-blocked-thread)
                (channel-put resp-chan #t)
                (add-running-thread thread))
              (hash-remove! signal-waiters a-signal)]
             [#t
              (void)]
             [#f
              (error 'non-constructive! "(not really; we need to roll back here)")])
           (loop)))
        (handle-evt
         par-start-chan
         (λ (parent+children-threads)
           (match-define (cons parent-thread children-threads) parent+children-threads)
           (add-running-threads children-threads)
           (remove-running-thread parent-thread)
           (hash-set! par-children parent-thread children-threads)
           (loop)))
        (handle-evt
         par-partly-done-chan
         (λ (parent-thread+done-thread)
           (match-define (cons parent-thread done-thread) parent-thread+done-thread)
           (remove-running-thread done-thread)
           (define parents-new-children
             (set-remove (hash-ref par-children parent-thread) done-thread))
           (cond
             [(set-empty? parents-new-children)
              (hash-remove! par-children parent-thread)
              (add-running-thread parent-thread)]
             [else
              (hash-set! par-children parent-thread parents-new-children)])
           (loop)))
        (handle-evt
         pause-chan
         (λ (thread+resp-chan)
           (match-define (cons paused-thread resp-chan) thread+resp-chan)
           (remove-running-thread paused-thread)
           (hash-set! paused-threads paused-thread resp-chan)
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
           (remove-running-thread reaction-thread)
           (loop)))
        )])))
