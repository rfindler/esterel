#lang racket/base
(require "private/full.rkt" "kernel.rkt"
         racket/contract
         (only-in "private/kernel.rkt" par/proc exn:fail:instantaneous-loop?)
         (for-syntax racket/base syntax/parse syntax/for-body))
(provide (all-from-out "kernel.rkt")
         halt loop abort sustain await every
         for/par for*/par
         (contract-out
          [exn:fail:instantaneous-loop? (-> any/c boolean?)]))

(begin-for-syntax
  (define (mk-for/par for_/fold/derived original-stx)
    (with-syntax ([for_/fold/derived for_/fold/derived])
      (syntax-parse original-stx
        [(_ clauses body ... expr)
         (with-syntax ([original-stx original-stx]
                       [((pre-body ...) (post-body ...))
                        (split-for-body original-stx #'(body ... expr))])
           #'(par/proc
              (for_/fold/derived
               original-stx ([thunks '()]) clauses
               pre-body ...
               (cons (λ () post-body ...) thunks))))]))))

(define-syntax (for/par stx) (mk-for/par #'for/fold/derived stx))
(define-syntax (for*/par stx) (mk-for/par #'for*/fold/derived stx))
