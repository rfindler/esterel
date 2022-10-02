#lang racket
(require "esterel.rkt" rackunit)

(check-equal?
 (react!
  (reaction
   (void)))
 (hash))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (signal-value s)))
   (hash s #f)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (emit s)
     (signal-value s)))
   (hash s #t)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par (begin (emit s) #f)
          (signal-value s))))
   (hash s #t)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par (signal-value s)
          (signal-value s))))
   (hash s #f)))

(check-equal?
 (react!
  (reaction
   (pause)))
 (hash))

(let ([s (signal)])
  (define r
    (reaction
     (pause)
     (emit s)))
  (check-equal?
   (react! r)
   (hash))
  (check-equal?
   (react! r)
   (hash s #t)))

(let ([s (signal)])
  (define r
    (reaction
     (pause)
     (emit s)
     (pause)))
  (check-equal?
   (react! r)
   (hash))
  (check-equal?
   (react! r)
   (hash s #t))
  (check-equal?
   (react! r)
   (hash)))

(let ([s (signal)])
  (define r
    (reaction
     (signal-value s)
     (pause)
     (emit s)
     (pause)
     (signal-value s)))
  (check-equal?
   (react! r)
   (hash s #f))
  (check-equal?
   (react! r)
   (hash s #t))
  (check-equal?
   (react! r)
   (hash s #f)))


(check-equal?
 (react! (reaction
          (par (pause)
               (void))))
 (hash))

(check-equal?
 (react! (reaction
          (par (pause)
               (pause))))
 (hash))

(let ([s (signal)])
  (check-equal?
   (react! (reaction
            (par (pause)
                 (emit s))))
   (hash s #t)))

(check-equal?
 (react! (reaction
          (par (par (pause)
                    (pause))
               (par (pause)
                    (pause)))))
 (hash))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par (par (signal-value s)
               (pause))
          (par (pause)
               (signal-value s)))))
   (hash s #f)))

(check-equal?
 (react! (reaction
          (par (par (par (par (pause)
                              (pause))
                         (par (pause)
                              (pause)))
                    (pause))
               (par (pause)
                    (par (par (pause)
                              (pause))
                         (par (pause)
                              (pause)))))))
 (hash))
