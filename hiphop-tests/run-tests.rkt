#lang racket

(require "find.rkt"
         "parse.rkt")
(define-logger hiphop)

; path-string? ->
; Runs the hiphop.js test at the given path or by the given name.
(define (run-hiphop-test path-or-name)
  (define path (hiphop-test->path path-or-name))
  (display (format "Running test ~a\n" path))
  (define p (load-hiphop-test path))
  (log-hiphop-debug "parsed program to ~a" (pretty-format p))
  (run-a-test p)
  (void))

; ->
(define (run-all-hiphop-tests)
  (for-each run-hiphop-test (find-all-hiphop-tests)))

(define (run-a-test p)
  (printf "wish I could run ~s\n" p))

(module+ test
  (run-all-hiphop-tests))
