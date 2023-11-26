#lang racket/base
(require "private/kernel.rkt"
         racket/math
         racket/contract
         racket/set)

(define (pre-cond-check)
  (cond
    [(in-esterel?) #t]
    [else
     "must be run from within the dynamic extent of `esterel`"]))

(provide
 esterel
 with-signal
 define-signal
 par
 suspend
 with-trap
 debug-when-must
 exec
 (contract-out
  [esterel? (-> any/c boolean?)]
  [react! (->* (esterel?)
               (#:emit (listof
                        (or/c (and/c signal? (not/c signal-combine))
                              (cons/c (and/c signal? signal-combine)
                                      any/c))))
               (hash/dc [s signal?]
                        ;; if a signal is not emitted
                        ;; and also has a combination function,
                        ;; then it just won't be in the map,
                        ;; so we won't be told that its absense
                        ;; mattered to the computation
                        [v (s) (if (signal-combine s)
                                   any/c
                                   boolean?)]
                        #:immutable #t #:kind 'flat))]
  [in-esterel? (-> boolean?)]

  ;; the uses of `values` here (and below) is to avoid
  ;; a bug in 8.8's racket/contract library; they can
  ;; be removed when 8.9 comes out
  [present? (values
             (->* (signal?)
                  (#:pre natural?)
                  #:pre/desc (pre-cond-check)
                  boolean?))]

  ;; when a signal is not emitted it will return the
  ;; previous instant's value from signal-value, following
  ;; _The ESTEREL synchronous programming language: design,
  ;; semantics, implementation*_ by Berry and Gonthier
  [signal-value (values
                 (->* ((and/c signal? signal-combine))
                      ;; NB when we go "too far" with #:pre the values are just #f,
                      ;; even if the signal never had that value... is this okay?
                      (#:pre natural?
                       #:can (set/c signal?))
                      #:pre/desc (pre-cond-check)
                      any/c))]
  [signal? (-> any/c boolean?)]
  [signal-name (-> signal? (or/c #f string?))]
  [signal-index (-> signal? (or/c #f natural?))]
  [signal-combine (-> signal? (or/c #f (-> any/c any/c any)))]
  [emit (values (->* (signal?)
                     (any/c)
                     #:pre/desc (pre-cond-check)
                     void?))]
  [pause (values (->* () #:pre/desc (pre-cond-check) void?))]
  [exit-trap (->* (trap?) (any/c) any)]
  [trap? (-> any/c boolean?)]
  [exn:fail:not-constructive? (-> any/c boolean?)]))
