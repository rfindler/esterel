#lang racket

;; Code for finding things:
;;  - Hop binaries
;;  - Hiphop.js libraries and tests

(require racket/runtime-path
         file/glob)
(provide install-prefix
         hop-binary-path
         hiphop-path
         hiphop-test-directory
         hiphop-lib-directory
         find-all-hiphop-tests
         hiphop-test-name->path
         hiphop-test->path)

; Where third-party software lives:
(define-runtime-path install-prefix
  (build-path 'up "install-prefix"))

; Where to find the hop binary:
(define hop-binary-path
  (make-parameter (build-path install-prefix "hop" "bin" "hop")
                  path-string?))

; Root of the hiphop.js distribution:
(define-runtime-path hiphop-path "hiphop")

; -> path-string?
; Where hiphop.js tests are located.
(define (hiphop-test-directory)
  (build-path hiphop-path "tests"))

; -> path-string?
; Where hiphop.js libraries are located.
(define (hiphop-lib-directory)
  (build-path (hiphop-path) "lib"))

; A SkipTable is [hashof string? [listof string?]
; interp. A skip table maps names of tests that should be skipped to the reason
; they should be skipped.

; port? -> SkipTable
; Reads the skip table from an input port.
(define (parse-skip-table port)
  (define result (make-hash))
  (let loop ()
    (define next-line (read-line port))
    (unless (eof-object? next-line)
      (define split (regexp-split #rx"\t+" next-line))
      (hash-set! result (first split) (second split))
      (loop)))
  result)

; File listing which tests to skip:
(define-runtime-path skip-file (build-path "skip.txt"))

; -> SkipTable
; Loads the skip table from "skip.txt"
(define (load-skip-table)
  (call-with-input-file skip-file parse-skip-table))

; SkipTable path-string? -> [or/c string? false?]
; Returns the reason to skip, or #false not to skip.
(define (skip? skiptab path)
  (hash-ref skiptab (hiphop-test-path->name path) #false))

; -> [List-of path-string?]
; A list of all hiphop.js tests to run.
(define (find-all-hiphop-tests)
  (define skip-table (load-skip-table))
  (for/list ([path (glob (build-path (hiphop-test-directory) "*.in"))]
             #:unless (skip? skip-table path))
    (path-replace-extension path ".js")))

; path? -> string?
(define (hiphop-test-path->name path)
  (define-values (_1 name _2) (split-path (path-replace-extension path "")))
  (path->string name))

; string? -> path?
; Converts the name of a hiphop.js test to its absolute path.
(define (hiphop-test-name->path name)
  (build-path (hiphop-test-directory) (path-replace-extension name ".js")))

; path-string? -> path?
; Converts either kind of test designator to the test path.
(define (hiphop-test->path path-or-name)
  (if (path? path-or-name)
      path-or-name
      (hiphop-test-name->path path-or-name)))
