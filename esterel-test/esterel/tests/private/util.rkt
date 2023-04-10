#lang racket
(require esterel/kernel)
(provide non-constructive-exn?)

(define (non-constructive-exn? x)
  (and (exn:fail:not-constructive? x)
       (regexp-match? #rx"not constructive" (exn-message x))))
