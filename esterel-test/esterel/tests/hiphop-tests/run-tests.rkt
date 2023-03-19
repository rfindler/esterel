#lang racket

(require "find.rkt"
         "parse.rkt"
         "convert.rkt")

(define (run-all-hiphop-tests)
  (define any-failed? #f)
  (for ([test (in-list (find-all-hiphop-tests))])
    (define-values (base name dir?) (split-path test))
    (printf "running ~a\n" name)
    (with-handlers ([exn:fail? (λ (x)
                                 (set! any-failed? #t)
                                 ((error-display-handler)
                                  (exn-message x)
                                  x))])
      (define failed? (run-a-test test))
      (set! any-failed? (or failed? any-failed?))))
  (if any-failed?
      (error 'run-all-hiphop-tests "some tests failed")
      (printf "all tests passed\n")))

(define (run-a-test p) (run-hiphop-test (load-hiphop-test p)))

(module+ test
  (run-all-hiphop-tests))
