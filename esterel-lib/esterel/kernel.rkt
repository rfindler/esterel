#lang racket/base
(require "private/kernel.rkt"
         "private/compound-signals.rkt"
         "private/structs.rkt"
         racket/math
         racket/contract
         racket/set)

(define (pre-cond-check)
  (cond
    [(in-esterel?) #t]
    [else
     "must be run from within the dynamic extent of `esterel`"]))

(define (pre-cond-check-outside-esterel)
  (cond
    [(in-esterel?)
     "must be run from outside the dynamic extent of `esterel`"]
    [else #t]))

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
                        (or/c (and/c atomic-signal? (not/c atomic-signal-combine))
                              (cons/c (and/c atomic-signal? atomic-signal-combine)
                                      any/c))))
               (hash/dc [s atomic-signal?]
                        ;; if a signal is not emitted
                        ;; and also has a combination function,
                        ;; then it just won't be in the map,
                        ;; so we won't be told that its absense
                        ;; mattered to the computation
                        [v (s) (if (atomic-signal-combine s)
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

  [make-global-signal (->* (string?)
                           (#:init any/c #:combine any/c #:memoryless? any/c)
                           #:pre/desc (pre-cond-check-outside-esterel)
                           atomic-signal?)]
  
  ;; when a signal is not emitted it will return the
  ;; previous instant's value from signal-value, following
  ;; _The ESTEREL synchronous programming language: design,
  ;; semantics, implementation*_ by Berry and Gonthier
  [signal-value (values
                 (->* ((and/c atomic-signal? atomic-signal-combine))
                      ;; NB when we go "too far" with #:pre the values are just #f,
                      ;; even if the signal never had that value... is this okay?
                      (#:pre natural?
                       #:can (set/c atomic-signal?))
                      #:pre/desc (pre-cond-check)
                      any/c))]
  [signal? (-> any/c boolean?)]
  [atomic-signal? (-> any/c boolean?)]
  [signal-and (->* (signal?) #:rest (listof signal?) compound-signal?)]
  [signal-or (->* (signal?) #:rest (listof signal?) compound-signal?)]
  [signal-not (-> signal? compound-signal?)]
  [signal-name (-> signal? (flat-rec-contract
                            signal-name?
                            (cons/c 'or (listof signal-name?))
                            (cons/c 'and (listof signal-name?))
                            (list/c 'not signal-name?)
                            (or/c (and/c string? immutable?)
                                  #f)))]
  [signal-index (-> atomic-signal? (or/c #f natural?))]
  [signal-combine (-> atomic-signal? (or/c #f (-> any/c any/c any)))]
  [emit (values (->* (atomic-signal?)
                     (any/c)
                     #:pre/desc (pre-cond-check)
                     void?))]
  [pause (values (->* () #:pre/desc (pre-cond-check) void?))]
  [exit-trap (->* (trap?) (any/c) any)]
  [trap? (-> any/c boolean?)]
  [exn:fail:not-constructive? (-> any/c boolean?)]))

(define (signal-combine s) (atomic-signal-combine s))
