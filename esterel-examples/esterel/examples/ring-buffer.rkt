#lang racket
(require esterel/kernel esterel/full rackunit)

;; public interface
(define-signal rem-s add-s top-s #:single inp-s #:single)

#|

Q: top should be the value next to be removed, unless
the FIFO is empty, in which case ...?

A: the top-s should always be "the thing that got removed" and
if nothing got removed, well, we don't care.

also: add an err-s when someone adds to a full or removes from an empty buffer

another idea: what about "find" operation that searches through the FIFO
for something.

|#

;; private state
(define-signal in-s #:single out-s #:single buffer-s #:single size-s #:single)

(define (ring-buffer max-size)
  (emit in-s 0)
  (emit out-s 0)
  (emit size-s 0)
  (emit buffer-s (vector->immutable-vector (make-vector max-size #f)))
  (pause)
    
  (loop
   (par
      
    (let ()
      (define buffer (signal-value buffer-s #:can (set top-s size-s out-s)))
      (define size (signal-value size-s #:can (set top-s out-s)))
      (cond
        [(= 0 size)
         (emit top-s #f)]
        [else
         (define out (signal-value out-s #:can (set top-s)))
         (emit top-s (vector-ref buffer (signal-value out-s #:can (set top-s))))]))

    (cond
      [(and (present? add-s) (present? rem-s))
       (define in (signal-value in-s #:pre 1))
       (define out (signal-value out-s #:pre 1))
       (emit size-s (signal-value size-s #:pre 1))
       (emit in-s (modulo (+ in 1) max-size))
       (emit out-s (modulo (+ out 1) max-size))
       (define after-rem
         (vector-set/copy (signal-value buffer-s #:pre 1)
                          out
                          #f))
       (define after-add
         (vector-set/copy after-rem
                          in
                          (signal-value inp-s #:can (set buffer-s))))
       (emit buffer-s after-add)]
      [(present? add-s)
       (define i (signal-value in-s #:pre 1))
       (emit in-s (modulo (+ i 1) max-size))
       (emit out-s (signal-value out-s #:pre 1))
       (emit size-s (+ (signal-value size-s #:pre 1) 1))
       (emit buffer-s
             (vector-set/copy (signal-value buffer-s #:pre 1)
                              i
                              (signal-value inp-s #:can (set buffer-s))))]
      [(present? rem-s)
       (define i (signal-value out-s #:pre 1))
       (emit size-s (- (signal-value size-s #:pre 1) 1))
       (emit in-s (signal-value in-s #:pre 1))
       (emit out-s (modulo (+ i 1) max-size))
       (emit buffer-s
             (vector-set/copy (signal-value buffer-s #:pre 1)
                              i
                              #f))]))

   (pause)))

(define (mk size)
  (define r (esterel #:pre 1 (ring-buffer size)))
  (react! r)
  r)

(define (add rb what)
  (get-state (react! rb #:emit (list add-s (cons inp-s what)))))

(define (rem rb)
  (get-state (react! rb #:emit (list rem-s))))

(define (add-rem rb what)
  (get-state (react! rb #:emit (list add-s rem-s (cons inp-s what)))))

;; holds the current state to simplify test cases
(struct state (top in out buffer size) #:transparent)

(define (get-state r)
  (state (hash-ref r top-s)
         (hash-ref r in-s #f)
         (hash-ref r out-s #f)
         (hash-ref r buffer-s #f)
         (hash-ref r size-s #f)))

(define rb (mk 3))

(check-equal? (add rb "a") (state "a" 1 0 #("a" #f #f) 1))
(check-equal? (add rb "b") (state "a" 2 0 #("a" "b" #f) 2))
(check-equal? (add rb "c") (state "a" 0 0 #("a" "b" "c") 3))
(check-equal? (rem rb) (state "b" 0 1 #(#f "b" "c") 2))
(check-equal? (rem rb) (state "c" 0 2 #(#f #f "c") 1))
(check-equal? (rem rb) (state #f 0 0 #(#f #f #f) 0))
(check-equal? (add rb "d") (state "d" 1 0 #("d" #f #f) 1))
(check-equal? (add-rem rb "e") (state "e" 2 1 #(#f "e" #f) 1))
(check-equal? (add rb "f") (state "e" 0 1 #(#f "e" "f") 2))
(check-equal? (add rb "g") (state "e" 1 1 #("g" "e" "f") 3))
(check-equal? (add-rem rb "h") (state "f" 2 2 #("g" "h" "f") 3))
(check-equal? (rem rb) (state "g" 2 0 #("g" "h" #f) 2))
(check-equal? (rem rb) (state "h" 2 1 #(#f "h" #f) 1))
(check-equal? (rem rb) (state #f 2 2 #(#f #f #f) 0))
(check-equal? (add-rem rb "i") (state #f 0 0 #(#f #f #f) 0))
