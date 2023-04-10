#lang racket
(require esterel/kernel rackunit)

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (esterel
     (car #f)))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (esterel
     (par
      (car #f))))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (esterel
     (par
      (pause)
      (car #f))))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (esterel
     (with-trap T
       (par
        (exit-trap T)
        (car #f)))))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (esterel
     (with-trap T
       (par
        (par
         (exit-trap T))
        (par
         (car #f))))))))

(with-signal (S)
  (define r
    (esterel
     (with-trap T
       (par
        (par
         (pause) (emit S))
        (par
         (car #f))))))
  (check-exn
   #rx"expected: pair[?].* given: #f"
   (λ () (react! r)))
  (check-exn
   #rx"expected: pair[?].* given: #f"
   (λ () (react! r))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (with-signal (s1 s2)
     (react!
      (esterel
       (par
        (if (present? s1)
            (void)
            (car #f))
        (if (present? s2)
            (void)
            (car #f))))))))

;; exceptions raised during can exploration
;; "don't count" in the sense that we just
;; stop tracking signal emission in that par
;; only if the exception is raised in must mode
;; do we propagate it out of the esterel context
(with-signal (S)
  (check-equal?
   (react!
    (esterel
     (if (present? S)
         (car #f)
         (void))))
   (hash S #f)))
