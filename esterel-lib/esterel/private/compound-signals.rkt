#lang racket
(require "structs.rkt")

(provide signal-name compute-signal-presence
         signal-and signal-or signal-not)

(define (signal-name cs)
  (cond
    [(compound-signal? cs) ((compound-signal-get-name cs))]
    [(atomic-signal? cs) (atomic-signal-name cs)]))

(define (compute-signal-presence cs signal-status)
  (cond
    [(compound-signal? cs) ((compound-signal-get-value/dependencies cs) signal-status)]
    [(atomic-signal? cs)
     (cond
       [(hash-has-key? signal-status cs) (hash-ref signal-status cs)]
       [else (set cs)])]))

(define (signal-and s1 . ss)
  (compound-signal
   (λ () `(and ,@(for/list ([s (in-list (cons s1 ss))])
                   (signal-name s))))
   (λ (signal-status)
     (let loop ([unknown (set)]
                [children (cons s1 ss)])
       (cond
         [(empty? children)
          (if (set-empty? unknown)
              #t
              unknown)]
         [else
          (match (compute-signal-presence (car children) signal-status)
            [#f #f]
            [#t (loop unknown (cdr children))]
            [(? set? s) (loop (set-union unknown s) (cdr children))])])))))

(define (signal-or s1 . ss)
  (compound-signal
   (λ () `(or ,@(for/list ([s (in-list (cons s1 ss))])
                   (signal-name s))))
   (λ (signal-status)
     (let loop ([unknown (set)]
                [children (cons s1 ss)])
       (cond
         [(empty? children)
          (if (set-empty? unknown)
              #f
              unknown)]
         [else
          (match (compute-signal-presence (car children) signal-status)
            [#t #t]
            [#f (loop unknown (cdr children))]
            [(? set? s) (loop (set-union unknown s) (cdr children))])])))))

(define (signal-not s)
  (compound-signal
   (λ () `(not ,(signal-name s)))
   (λ (signal-status)
     (match (compute-signal-presence s signal-status)
       [#t #f]
       [#f #t]
       [(? set? u) u]))))
