#lang racket/base
(require racket/hash-code)

(define (mk-write-proc get-name what)
  (define (write-proc a-signal port mode)
    (define name (get-name a-signal))
    (cond
      [name
       (display "#<" port)
       (display what port)
       (display ": " port)
       (display name port)
       (display ">" port)]
      [else
       (display "#<" port)
       (display what port)
       (display ">" port)]))
  write-proc)

;; identity controls the equivalence relation on signals
;; if it is #f, then the comparison is just like a regular
;; opaque struct, namely based on each allocation being different
;; from all the others. This works fine when we're not in a
;; reaction, but when we are, we might create a signal during
;; a can exploration. In that case, we should create the same
;; signal during a later must run. See `mk-signal/args` for
;; how that is done
;; If the identity is not #t, it is expected to be a pair
;; whose car component is might be useful as debugging
;; information so it printed as part of the signal
;;
;; combine :
;; (or/c #f          -- not a valued signal
;;       'single     -- a valued signal that can only be emitted once per instant
;;       (α α -> α)) -- a (hopefully) associative, commutative operation
;;
;; suspensions : (set/c suspension)
;;  the suspensions are the set of `suspension` structs corresponding
;;  to the suspensions that this signal is created in the scope of
(struct atomic-signal (name identity init combine suspensions)
  #:methods gen:custom-write
  [(define write-proc
     (mk-write-proc
      (λ (x)
        (if (atomic-signal-identity x)
            (format "~a (~s)" (atomic-signal-name x) (car (atomic-signal-identity x)))
            (atomic-signal-name x)))
      "signal"))]
  #:methods gen:equal-mode+hash
  [(define (equal-mode-proc self other rec mode)
     (cond
       [(atomic-signal? other)
        (define self-id (atomic-signal-identity self))
        (define other-id (atomic-signal-identity other))
        (cond
          [(and self-id other-id)
           ;; the identity of the signal is not a generic thing
           ;; inside a signal but instead something where the
           ;; predefined equal? is the correct comparison
           (equal? self-id other-id)]
          [else
           (eq? self other)])]
       [else #f]))
   (define (hash-mode-proc self rec mode)
     (define self-id (atomic-signal-identity self))
     (cond
       [self-id (hash-code-combine (equal-hash-code self-id))]
       [else (eq-hash-code self)]))])
(define (signal-index s)
  (define id (atomic-signal-identity s))
  (and id (car id)))

(struct memoryless-signal atomic-signal ())

(struct trap (name counter escape)
  #:methods gen:custom-write
  [(define write-proc (mk-write-proc (λ (x) (trap-name x)) "trap"))])

;; trap : trap?
;; vals : (set/c any/c)
;; when escaping, we pair a trap with a set of values; the trap is where we are
;; escaping to and the value is which values we'll return to the trap
(struct trap+vals (trap vals) #:transparent)

;; get-name : -> sexp [that names the signal]
;; get-value/dependencies :
;;     hash[atomic-signal -o> boolean]
;;  -> (or/c boolean?                         -- the signal is resolved
;;           (and/c (set/c atomic-signal?)    -- the signal needs these atomic signals to resolve
;;                  (not/c set-empty?)))
;; the argument to get-value/dependencies is expected
;; a table with the same signature as the `signal-status`
;; table from private/kernel.rkt
(struct compound-signal (get-name get-value/dependencies)
  #:methods gen:custom-write
  [(define write-proc
     (mk-write-proc
      (λ (x)
        ((compound-signal-get-name x)))
      "signal"))])

(define (signal? s) (or (atomic-signal? s) (compound-signal? s)))

;; the identity of these is important; each signal is
;; associated with a set of these and, when a suspend
;; happens (ie the second argument to `suspend` is true),
;; we'll note that these suspensions are active and thus
;; using any signals created inside them are not allowed
;; to be used.
(struct suspension (thunk) #:transparent)

(provide (struct-out atomic-signal)
         (struct-out compound-signal)
         (struct-out memoryless-signal)
         (struct-out suspension)
         signal?
         signal-index
         (struct-out trap)
         (struct-out trap+vals))
