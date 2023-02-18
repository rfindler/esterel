#lang racket

(require "find.rkt"
         "parse.rkt"
         "convert.rkt")

(define (run-all-hiphop-tests)
  (for ([test (in-list (find-all-hiphop-tests))])
    (define-values (base name dir?) (split-path test))
    (printf "running ~a\n" name)
    (with-handlers ([exn:fail? (λ (x)
                                 ((error-display-handler)
                                  (exn-message x)
                                  x))])
      (run-a-test test))))

(define (run-a-test p) (run-hiphop-test (load-hiphop-test p)))

(module+ test
  (run-all-hiphop-tests))