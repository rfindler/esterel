#lang racket
(require esterel/kernel esterel/full rackunit)

(check-true (with-signal (S) (signal? S)))
(check-true (with-signal (S) (signal? (signal-not S))))
(check-false (with-signal (S) (signal? "S")))
(check-true (with-signal (S) (atomic-signal? S)))
(check-false (with-signal (S) (atomic-signal? (signal-not S))))
(check-false (with-signal (S) (atomic-signal? "S")))

(check-true
 (immutable? (with-signal (S) (signal-name S))))

(with-signal (S)
  (define (signals->names ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (signal-name k) v)))
  (define r
    (esterel
     (present? S)
     (present? (with-signal (S2) S2))))
  (check-equal? (signals->names (react! r))
                (hash "S" #f  "S2" #f)))

(let ()
  (define (signals->names ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (signal-name k) v)))
  (define r
    (esterel
     (with-signal (S1)
       (present? S1)
       (with-signal (S2 O1 O2)
         (par (emit S2)
              (if (present? S2)
                  (emit O1)
                  (emit O2)))))))
  (check-equal? (signals->names (react! r))
                (hash "S1" #f "S2" #t "O1" #t)))

(let ()
  (define (signals->names ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (signal-name k) v)))
  (define r
    (esterel
     (with-signal (S1)
       (present? S1)
       (with-signal (S2 O1 O2)
         (if (present? S2)
             (emit O1)
             (emit O2))))))
  (check-equal? (signals->names (react! r))
                (hash "S1" #f "S2" #f "O2" #t)))

(let ()
  (define (signals->names ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (signal-name k) v)))
  (define r
    (esterel
     (with-signal (S1)
       (present? S1)
       (with-signal (S2 O1 O2)
         (if (present? S2)
             (emit O1)
             (emit O2))))))
  (check-equal? (signals->names (react! r))
                (hash "S1" #f "S2" #f "O2" #t)))


(let ()
  (define (signals->names ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (signal-name k) v)))
  (define r
    (esterel
     (with-signal (s1 s2)
       (present? s1)
       (emit s2))))
  (check-equal? (signals->names (react! r))
                (hash "s1" #f "s2" #t)))

(check-exn
 #rx"emit:.*dynamic extent"
 (λ ()
   (react!
    (esterel
     (emit (with-signal (s1) s1))))))

;; make sure that, when we're not in a esterel,
;; the last expression is in tail position
(check-equal?
 (continuation-mark-set->list
  (with-continuation-mark 'x 1
    (with-signal (S)
      (with-continuation-mark 'x 2
        (current-continuation-marks))))
  'x)
 (list 2))

(let ()
  (define (signals->names* ht)
    (for/fold ([h (hash)])
              ([(s v) (in-hash ht)])
      (define k (signal-name s))
      (hash-set h k (set-add (hash-ref h k set) v))))
  ;; we don't pick up the first S2 because
  ;; it doesn't affect the computation
  (check-equal?
   (with-signal (S)
     (signals->names*
      (react!
       (esterel
        (if (present? S)
            (with-signal (S2)
              (emit S2))
            (with-signal (S2)
              (if (present? S2)
                  (emit S)
                  (void))))))))
   (hash "S" (set #f)
         "S2" (set #f))))


;                                                                               
;                                                                               
;                                                                               
;                                                                               
;                                                                               
;                                                                           ;;; 
;                                                                           ;;; 
;     ;;;      ;;;    ;;; ;; ;;;    ;;; ;;     ;;;    ;;; ;;;  ;;; ;;    ;; ;;; 
;    ;;;;;    ;;;;;   ;;;;;;;;;;;   ;;;;;;;   ;;;;;   ;;; ;;;  ;;;;;;;  ;;;;;;; 
;   ;;; ;;;  ;;; ;;;  ;;; ;;; ;;;   ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;; 
;   ;;;      ;;; ;;;  ;;; ;;; ;;;   ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;; 
;   ;;;      ;;; ;;;  ;;; ;;; ;;;   ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;; 
;   ;;; ;;;  ;;; ;;;  ;;; ;;; ;;;   ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;; 
;    ;;;;;    ;;;;;   ;;; ;;; ;;;   ;;;;;;;   ;;;;;   ;;;;;;;  ;;; ;;;  ;;;;;;; 
;     ;;;      ;;;    ;;; ;;; ;;;   ;;; ;;     ;;;     ;; ;;;  ;;; ;;;   ;; ;;; 
;                                   ;;;                                         
;                                   ;;;                                         
;                                   ;;;                                         
;                                                                               
;                                                                               
;                                                         
;                                                         
;                                                         
;                                                         
;                                                         
;            ;;;                             ;;;          
;                                            ;;;          
;    ;;;;;   ;;;   ;; ;;;  ;;; ;;    ;;;;;   ;;;   ;;;;;  
;   ;;;  ;;  ;;;  ;;;;;;;  ;;;;;;;  ;;;;;;;  ;;;  ;;;  ;; 
;   ;;;      ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;;  ;;;     
;   ;;;;;;   ;;;  ;;; ;;;  ;;; ;;;     ;;;;  ;;;  ;;;;;;  
;    ;;;;;;  ;;;  ;;; ;;;  ;;; ;;;   ;; ;;;  ;;;   ;;;;;; 
;       ;;;  ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;;      ;;; 
;   ;;  ;;;  ;;;  ;;;;;;;  ;;; ;;;  ;;;;;;;  ;;;  ;;  ;;; 
;    ;;;;;   ;;;   ;; ;;;  ;;; ;;;   ;;;;;;  ;;;   ;;;;;  
;                     ;;;                                 
;                 ;;;;;;;                                 
;                  ;;;;;                                  
;                                                         
;                                                         

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1 s2)
       (if (present? (signal-and s1 s2))
           (emit s3)
           (void)))))
  (check-equal? (hash-ref (react! r) s3 #f) #f))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1 s2)
       (par (emit s1)
            (if (present? (signal-and s1 s2))
                (emit s3)
                (void))))))
  (check-equal? (hash-ref (react! r) s3 #f) #f))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1 s2)
       (par (emit s2)
            (if (present? (signal-and s1 s2))
                (emit s3)
                (void))))))
  (check-equal? (hash-ref (react! r) s3 #f) #f))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1 s2)
       (par (emit s1)
            (emit s2)
            (if (present? (signal-and s1 s2))
                (emit s3)
                (void))))))
  (check-equal? (hash-ref (react! r) s3 #f) #t))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1 s2)
       (if (present? (signal-or s1 s2))
           (emit s3)
           (void)))))
  (check-equal? (hash-ref (react! r) s3 #f) #f))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1 s2)
       (par (emit s1)
            (if (present? (signal-or s1 s2))
                (emit s3)
                (void))))))
  (check-equal? (hash-ref (react! r) s3 #f) #t))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1 s2)
       (par (emit s2)
            (if (present? (signal-or s1 s2))
                (emit s3)
                (void))))))
  (check-equal? (hash-ref (react! r) s3 #f) #t))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1 s2)
       (par (emit s1)
            (emit s2)
            (if (present? (signal-or s1 s2))
                (emit s3)
                (void))))))
  (check-equal? (hash-ref (react! r) s3 #f) #t))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1)
       (par (emit s1)
            (if (present? (signal-not s1))
                (emit s3)
                (void))))))
  (check-equal? (hash-ref (react! r) s3 #f) #f))

(with-signal (s3)
  (define r
    (esterel
     (with-signal (s1)
       (par (emit s1)
            (if (present? (signal-not s1))
                (void)
                (emit s3))))))
  (check-equal? (hash-ref (react! r) s3 #f) #t))

(check-exn
 #rx"not constructive"
 (λ ()
   (react!
    (esterel
     (with-signal (s1 s2)
       (if (and (present? s1) (present? s2))
           (void)
           (emit s1)))))))

(check-true
 (hash?
  (react!
   (esterel
    (with-signal (s1 s2)
      (if (present? (signal-and s1 s2))
          (void)
          (emit s1)))))))

(check-exn
 #rx"not constructive"
 (λ ()
   (react!
    (esterel
     (with-signal (s1 s2)
       (if (present? s1)
           (if (present? (signal-and (signal-not s1) s2))
               (void)
               (emit s1))
           (void)))))))

(check-exn
 #rx"not constructive"
 (λ ()
   (react!
    (esterel
     (with-signal (s1 s2)
       (if (present? s1)
           (if (present? (signal-and (signal-not s2) s2))
               (void)
               (emit s1))
           (void)))))))

(check-true
 (hash?
  (react!
   (esterel
    (with-signal (s1 s2)
      (if (present? s1)
          (if (present? (signal-and (signal-not s2) s2))
              (emit s1)
              (void))
          (void)))))))


(check-exn
 #rx"not constructive"
 (λ ()
   (react!
    (esterel
     (with-signal (s1)
       (if (present? (signal-and (signal-not s1) s1))
           (void)
           (emit s1)))))))

(check-exn
 #rx"not constructive"
 (λ ()
   (react!
    (esterel
     (with-signal (s1)
       (if (present? (signal-and (signal-not s1) s1))
           (emit s1)
           (void)))))))
