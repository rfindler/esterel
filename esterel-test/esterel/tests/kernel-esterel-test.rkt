#lang racket
(require esterel/kernel rackunit)

(define (non-constructive-exn? x)
  (and (exn:fail:not-constructive? x)
       (regexp-match? #rx"not constructive" (exn-message x))))

(check-equal?
 (react!
  (esterel
   (void)))
 (hash))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (present? s)))
   (hash s #f)))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (emit s)
     (present? s)))
   (hash s #t)))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (par
      (begin (emit s) #f)
      (present? s))))
   (hash s #t)))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (par
      (present? s)
      (present? s))))
   (hash s #f)))

(check-equal?
 (react!
  (esterel
   (pause)))
 (hash))

(with-signal (s)
  (define r
    (esterel
     (pause)
     (emit s)))
  (check-equal?
   (react! r)
   (hash))
  (check-equal?
   (react! r)
   (hash s #t)))

(with-signal (s)
  (define r
    (esterel
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

(with-signal (s)
  (define r
    (esterel
     (present? s)
     (pause)
     (emit s)
     (pause)
     (present? s)))
  (check-equal?
   (react! r)
   (hash s #f))
  (check-equal?
   (react! r)
   (hash s #t))
  (check-equal?
   (react! r)
   (hash s #f)))

;; test that suspension suspends signals
(with-signal (S2
              O #:init (set) #:combine set-union)
  (define r
    (esterel
     #:pre 1
     (suspend
      (with-signal (S1)
        (let loop ()
          (emit S1)
          (pause)
          (emit O (set (present? S1 #:pre 1)))
          (loop)))
      (present? S2))))
  (define (react!/names r #:emit [emitters '()])
    (for/hash ([(k v) (in-hash (react! r #:emit emitters))])
      (values (signal-name k) v)))
  (check-equal? (react!/names r) (hash "S1" #t))
  (check-equal? (react!/names r #:emit (list S2)) (hash "S2" #t))
  (check-equal? (react!/names r) (hash "S1" #t "O" (set #t) "S2" #f))
  (check-equal? (react!/names r) (hash "S1" #t "O" (set #t) "S2" #f))
  (check-equal? (react!/names r #:emit (list S2)) (hash "S2" #t))
  (check-equal? (react!/names r) (hash "S1" #t "O" (set #t) "S2" #f))
  (check-equal? (react!/names r) (hash "S1" #t "O" (set #t) "S2" #f)))

(with-signal (C #:combine (λ (x y) x) S2)
  (define r
    (esterel
     #:pre 1
     (par (suspend
           (with-signal (S1)
             (emit C S1)
             (let loop ()
               (emit S1)
               (pause)
               (loop)))
           (present? S2))
          (let ([S1 (signal-value C)])
            (pause)
            (emit S2)
            (sleep .01)
            (emit S1)))))
  (react! r)
  ;; this test will usually be the emit error but it isn't guaranteed;
  ;; one of these errors should be guaranteed, however.
  (check-exn #rx"(emit: signal is suspended)|(suspend: suspended signal was used)"
             (λ () (react! r))))

(with-signal (C #:combine (λ (x y) x) S2)
  (define r
    (esterel
     #:pre 1
     (par (suspend
           (with-signal (S1)
             (emit C S1)
             (let loop ()
               (emit S1)
               (pause)
               (loop)))
           (present? S2))
          (let ([S1 (signal-value C)])
            (pause)
            (emit S2)
            (sleep .01)
            (present? S1)))))
  (react! r)
  ;; this test will usually be the emit error but it isn't guaranteed;
  ;; one of these errors should be guaranteed, however.
  (check-exn #rx"(present[?]: signal is suspended)|(suspend: suspended signal was used)"
             (λ () (react! r))))

(with-signal (C #:combine (λ (x y) x) S2)
  (define r
    (esterel
     #:pre 1
     (par (suspend
           (with-signal (S1 #:combine +)
             (emit C S1)
             (let loop ()
               (emit S1 0)
               (pause)
               (loop)))
           (present? S2))
          (let ([S1 (signal-value C)])
            (pause)
            (emit S2)
            (sleep .01)
            (signal-value S1)))))
  (react! r)
  ;; this test will usually be the emit error but it isn't guaranteed;
  ;; one of these errors should be guaranteed, however.
  (check-exn #rx"(signal-value: signal is suspended)|(suspend: suspended signal was used)"
             (λ () (react! r))))

(with-signal (C #:combine (λ (x y) x) S2)
  (define r
    (esterel
     #:pre 1
     (par (suspend
           (with-signal (S1)
             (emit C S1)
             (let loop ()
               (emit S1)
               (pause)
               (loop)))
           (present? S2))
          (let ([S1 (signal-value C)])
            (pause)
            (emit S1)
            (sleep .01)
            (emit S2)))))
  (react! r)
  ;; this test will usually be the suspend error but it isn't guaranteed;
  ;; one of these errors should be guaranteed, however.
  (check-exn #rx"(emit: signal is suspended)|(suspend: suspended signal was used)"
             (λ () (react! r))))


;                                           
;                                           
;                                           
;                                           
;                                           
;                                   ;;;;;   
;                                  ;;;;;;;  
;   ;;; ;;    ;;;;;   ;;; ;;       ;;; ;;;  
;   ;;;;;;;  ;;;;;;;  ;;;;;        ;;; ;;   
;   ;;; ;;;  ;;; ;;;  ;;;           ;;;     
;   ;;; ;;;     ;;;;  ;;;         ;;;;;;;;; 
;   ;;; ;;;   ;; ;;;  ;;;        ;;;  ;;;;  
;   ;;; ;;;  ;;; ;;;  ;;;        ;;;   ;;;; 
;   ;;;;;;;  ;;;;;;;  ;;;        ;;;;;;;;;; 
;   ;;; ;;    ;;;;;;  ;;;         ;;;;;  ;; 
;   ;;;                                     
;   ;;;                                     
;   ;;;                                     
;                                           
;                                
;                                
;     ;                          
;   ;;;                          
;  ;;;;;; ;;; ;; ;;;;;   ;;; ;;  
;  ;;;;;; ;;;;; ;;;;;;;  ;;;;;;; 
;   ;;;   ;;;   ;;; ;;;  ;;; ;;; 
;   ;;;   ;;;      ;;;;  ;;; ;;; 
;   ;;;   ;;;    ;; ;;;  ;;; ;;; 
;   ;;;   ;;;   ;;; ;;;  ;;; ;;; 
;   ;;;;; ;;;   ;;;;;;;  ;;;;;;; 
;    ;;;; ;;;    ;;;;;;  ;;; ;;  
;                        ;;;     
;                        ;;;     
;                        ;;;     
;                                


(check-equal?
 (react! (esterel
          (par
           (pause)
           (void))))
 (hash))

(check-equal?
 (react! (esterel
          (par
           (pause)
           (pause))))
 (hash))

(with-signal (s)
  (check-equal?
   (react! (esterel
            (par
             (pause)
             (emit s))))
   (hash s #t)))

(check-equal?
 (react! (esterel
          (par
           (par
            (pause)
            (pause))
           (par
            (pause)
            (pause)))))
 (hash))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (par
      (par
       (present? s)
       (pause))
      (par
       (pause)
       (present? s)))))
   (hash s #f)))

(check-equal?
 (react! (esterel
          (par
           (par
            (par
             (par
              (pause)
              (pause))
             (par
              (pause)
              (pause)))
            (pause))
           (par
            (pause)
            (par
             (par
              (pause)
              (pause))
             (par
              (pause)
              (pause)))))))
 (hash))

(with-signal (s1 s2)
  (define r
    (esterel
     (par
      (let loop ()
        (suspend (begin (pause) (emit s2))
                 (present? s1))
        (loop))
      (begin (emit s1) (pause) (emit s1)))))
  (check-equal? (react! r) (hash s1 #t))
  (check-equal? (react! r) (hash s1 #t))
  (check-equal? (react! r) (hash s1 #f s2 #t))
  (check-equal? (react! r) (hash s1 #f s2 #t)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (with-trap t
       (emit s1)
       (exit-trap t)
       (emit s2))))
   (hash s1 #t)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (with-trap t
       (par
        (emit s1)
        (exit-trap t))
       (emit s2))))
   (hash s1 #t)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (with-trap t1
       (with-trap t2
         (par
          (exit-trap t1)
          (exit-trap t2))
         (emit s1))
       (emit s2))))
   (hash)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (with-trap t1
       (with-trap t2
         (with-trap t3
           (with-trap t4
             (par
              (par
               (exit-trap t1)
               (exit-trap t4))
              (par
               (exit-trap t2)
               (emit s1))))))
       (emit s2))))
   (hash s1 #t)))

(with-signal (s1 s2)
  (define r
    (esterel
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash)))

(with-signal (s1 s2 s3)
  (define r
    (esterel
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))
     (pause)
     (emit s3)))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash s3 #t)))

(let ()
  (define r
    (esterel
     (with-trap T1
       (par
        (exit-trap T1)))
     (par
      (pause)
      (pause))))

  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash)))


(with-signal (i o)
  (define r
    (esterel
     (let loop ()
       (when (present? i)
         (emit o))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash i #f))
  (check-equal? (react! r) (hash i #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t))
  (check-equal? (react! r) (hash i #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t)))

(with-signal (O)
  (define t
    (esterel
     (with-trap T
       (par
        (pause)
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(with-signal (O)
  (define t
    (esterel
     (with-trap T
       (par
        (par
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(with-signal (O)
  (define t
    (esterel
     (with-trap T
       (par
        (par
         (pause)
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(with-signal (O)
  (define t
    (esterel
     (with-trap T
       (par
        (par
         (pause)
         (void))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (par
      (with-trap T
        (par
         (par
          (pause))
         (exit-trap T)))
      (pause))
     (emit O)))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (par
      (with-trap T1
        (par
         (par
          (pause))
         (exit-trap T1)))
      (pause)
      (with-trap T2
        (par
         (par
          (pause))
         (exit-trap T2))))
     (emit O)))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (with-trap T0
       (par
        (with-trap T1
          (par
           (par
            (pause))
           (exit-trap T1)))
        (exit-trap T0)
        (with-trap T2
          (par
           (par
            (pause))
           (exit-trap T2))))
       (emit O))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash)))

(with-signal (O)
  (define r
    (esterel
     (with-trap T0
       (par
        (with-trap T1
          (par
           (par
            (pause))
           (exit-trap T1)))
        (exit-trap T0)
        (with-trap T2
          (par
           (par
            (pause))
           (exit-trap T2)))))
     (emit O)))
  (check-equal? (react! r) (hash O #t))
  (check-equal? (react! r) (hash)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the following tests add a bunch of calls to `(present? can-explore)`
;; in order to test can mode in the same situations as above
(define-signal can-explore)

(check-equal?
 (react! (esterel
          (present? can-explore)
          (par
           (pause)
           (void))))
 (hash can-explore #f))

(check-equal?
 (react! (esterel
          (present? can-explore)
          (par
           (pause)
           (pause))))
 (hash can-explore #f))

(with-signal (s)
  (check-equal?
   (react! (esterel
            (present? can-explore)
            (par
             (pause)
             (emit s))))
   (hash s #t can-explore #f)))

(check-equal?
 (react! (esterel
          (present? can-explore)
          (par
           (par
            (pause)
            (pause))
           (par
            (pause)
            (pause)))))
 (hash can-explore #f))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (par
      (par
       (present? s)
       (pause))
      (par
       (pause)
       (present? s)))))
   (hash s #f can-explore #f)))

(check-equal?
 (react! (esterel
          (present? can-explore)
          (par
           (par
            (par
             (par
              (pause)
              (pause))
             (par
              (pause)
              (pause)))
            (pause))
           (par
            (pause)
            (par
             (par
              (pause)
              (pause))
             (par
              (pause)
              (pause)))))))
 (hash can-explore #f))

(with-signal (s1 s2)
  (define r
    (esterel
     (present? can-explore)
     (par
      (let loop ()
        (suspend (begin (pause) (present? can-explore) (emit s2))
                 (present? s1))
        (loop))
      (begin (emit s1) (pause) (emit s1)))))
  (check-equal? (react! r) (hash s1 #t can-explore #f))
  (check-equal? (react! r) (hash s1 #t))
  (check-equal? (react! r) (hash s1 #f s2 #t can-explore #f))
  (check-equal? (react! r) (hash s1 #f s2 #t can-explore #f)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (with-trap t
       (emit s1)
       (exit-trap t)
       (emit s2))))
   (hash s1 #t can-explore #f)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (with-trap t
       (par
        (emit s1)
        (exit-trap t))
       (emit s2))))
   (hash s1 #t can-explore #f)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (with-trap t1
       (with-trap t2
         (par
          (exit-trap t1)
          (exit-trap t2))
         (emit s1))
       (emit s2))))
   (hash can-explore #f)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (with-trap t1
       (with-trap t2
         (with-trap t3
           (with-trap t4
             (par
              (par
               (exit-trap t1)
               (exit-trap t4))
              (par
               (exit-trap t2)
               (emit s1))))))
       (emit s2))))
   (hash s1 #t can-explore #f)))

(with-signal (s1 s2)
  (define r
    (esterel
     (present? can-explore)
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash)))

(with-signal (s1 s2 s3)
  (define r
    (esterel
     (present? can-explore)
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))
     (pause)
     (emit s3)))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash s3 #t)))

(let ()
  (define r
    (esterel
     (with-trap T1
       (par
        (exit-trap T1)
        (present? can-explore)))
     (par
      (pause)
      (pause))))

  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash)))


(with-signal (i o)
  (define r
    (esterel
     (let loop ()
       (present? can-explore)
       (when (present? i)
         (emit o))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash i #f can-explore #f))
  (check-equal? (react! r) (hash i #f can-explore #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t can-explore #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t can-explore #f))
  (check-equal? (react! r) (hash i #f can-explore #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t can-explore #f)))

(with-signal (O)
  (define t
    (esterel
     (present? can-explore)
     (with-trap T
       (par
        (pause)
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t can-explore #f)))

(with-signal (O)
  (define t
    (esterel
     (present? can-explore)
     (with-trap T
       (par
        (par
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t can-explore #f)))

(with-signal (O)
  (define t
    (esterel
     (present? can-explore)
     (with-trap T
       (par
        (par
         (pause)
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t can-explore #f)))

(with-signal (O)
  (define t
    (esterel
     (present? can-explore)
     (with-trap T
       (par
        (par
         (pause)
         (void))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t can-explore #f)))

(with-signal (O)
  (define r
    (esterel
     (present? can-explore)
     (par
      (with-trap T
        (par
         (par
          (begin (pause) (present? can-explore)))
         (exit-trap T)))
      (pause))
     (emit O)))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (present? can-explore)
     (par
      (with-trap T1
        (par
         (par
          (begin (pause) (present? can-explore)))
         (exit-trap T1)))
      (pause)
      (with-trap T2
        (par
         (par
          (begin (pause) (present? can-explore)))
         (exit-trap T2))))
     (emit O)))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (present? can-explore)
     (with-trap T0
       (par
        (with-trap T1
          (par
           (par
            (begin (pause) (present? can-explore)))
           (exit-trap T1)))
        (exit-trap T0)
        (with-trap T2
          (par
           (par
            (begin (pause) (present? can-explore)))
           (exit-trap T2))))
       (emit O))))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash)))

(with-signal (O)
  (define r
    (esterel
     (present? can-explore)
     (with-trap T0
       (par
        (with-trap T1
          (par
           (par
            (begin (pause) (present? can-explore)))
           (exit-trap T1)))
        (exit-trap T0)
        (with-trap T2
          (par
           (par
            (begin (pause) (present? can-explore)))
           (exit-trap T2)))))
     (emit O)))
  (check-equal? (react! r) (hash O #t can-explore #f))
  (check-equal? (react! r) (hash)))

;; P2
(let ()
  (define r
    (with-signal (O)
      (esterel
       (with-signal (S)
         (emit S)
         (when (present? O)
           (when (present? S)
             (pause))
           (emit O))))))
  (define (signals->names ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (signal-name k) v)))
  (check-equal? (signals->names (react! r))
                (hash "O" #f "S" #t)))
          

;                                      
;                                      
;                                      
;                                      
;                                      
;                                      
;                                      
;     ;;;   ;;;   ;;; ;;; ;;    ;;;;;  
;    ;;;;;   ;;; ;;;  ;;;;;;;  ;;;  ;; 
;   ;;; ;;;   ;;;;;   ;;; ;;;  ;;;     
;   ;;; ;;;   ;;;;;   ;;; ;;;  ;;;;;;  
;   ;;;;;;;   ;;;;;   ;;; ;;;   ;;;;;; 
;   ;;;      ;;; ;;;  ;;; ;;;      ;;; 
;    ;;;;;;  ;;; ;;;  ;;; ;;;  ;;  ;;; 
;     ;;;;  ;;;   ;;; ;;; ;;;   ;;;;;  
;                                      
;                                      
;                                      
;                                      
;                                      


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


;                                        
;                                        
;                                        
;                                        
;                                        
;     ;; ;;                              
;     ;; ;;                              
;  ;;;;;;;;  ;;;  ;;; ;;   ;;; ;;  ;;;   
;  ;;;;;;;;  ;;;  ;;;;;;;  ;;;;;  ;;;;;  
;    ;; ;;   ;;;  ;;; ;;;  ;;;   ;;; ;;; 
;    ;; ;;        ;;; ;;;  ;;;   ;;; ;;; 
;  ;;;;;;;;       ;;; ;;;  ;;;   ;;;;;;; 
;  ;;;;;;;;  ;;;  ;;; ;;;  ;;;   ;;;     
;   ;; ;;    ;;;  ;;;;;;;  ;;;    ;;;;;; 
;   ;; ;;    ;;;  ;;; ;;   ;;;     ;;;;  
;                 ;;;                    
;                 ;;;                    
;                 ;;;                    
;                                        
;                                        

(with-signal (S O)
  (define r
    (esterel
     #:pre 2
     (emit S)
     (pause)
     (when (present? S #:pre 1)
       (emit O))))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t)))

(with-signal (S O1 O2)
  (define r
    (esterel
     #:pre 2
     (if (present? S #:pre 1)
         (emit O1)
         (emit O2))))
  (check-equal? (react! r) (hash O2 #t)))

(with-signal (O)
  (define r
    (esterel
     #:pre 1
     (pause)
     (pause)
     (present? O #:pre 2)))
  (react! r)
  (react! r)
  (check-exn
   #rx"present[?]: #:pre argument too large.*maximum: 1"
   (λ () (react! r))))

(with-signal (S O)
  (define r
    (esterel
     #:pre 1
     (pause)
     (par
      (let loop () (emit S) (pause) (pause) (loop))
      (let loop ()
        (when (present? S #:pre 1)
          (emit O))
        (pause)
        (loop)))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t)))


;; test that with-signal doesn't break up
;; its but keeps everything in a single scope
(check-equal? (with-signal (S)
                (define (g) (f))
                (begin
                  (define (f) 1)
                  (g)))
              1)

;                                                
;                                                
;                                                
;                                                
;                                                
;            ;;;                             ;;; 
;                                            ;;; 
;    ;;;;;   ;;;   ;; ;;;  ;;; ;;    ;;;;;   ;;; 
;   ;;;  ;;  ;;;  ;;;;;;;  ;;;;;;;  ;;;;;;;  ;;; 
;   ;;;      ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; 
;   ;;;;;;   ;;;  ;;; ;;;  ;;; ;;;     ;;;;  ;;; 
;    ;;;;;;  ;;;  ;;; ;;;  ;;; ;;;   ;; ;;;  ;;; 
;       ;;;  ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; 
;   ;;  ;;;  ;;;  ;;;;;;;  ;;; ;;;  ;;;;;;;  ;;; 
;    ;;;;;   ;;;   ;; ;;;  ;;; ;;;   ;;;;;;  ;;; 
;                     ;;;                        
;                 ;;;;;;;                        
;                  ;;;;;                         
;                                                
;                                                
;                                                    
;                                                    
;                                                    
;                     ;;;                            
;                     ;;;                            
;  ;;;   ;;;  ;;;;;   ;;;  ;;; ;;;    ;;;     ;;;;;  
;   ;;   ;;  ;;;;;;;  ;;;  ;;; ;;;   ;;;;;   ;;;  ;; 
;   ;;; ;;;  ;;; ;;;  ;;;  ;;; ;;;  ;;; ;;;  ;;;     
;   ;;; ;;;     ;;;;  ;;;  ;;; ;;;  ;;; ;;;  ;;;;;;  
;    ;; ;;    ;; ;;;  ;;;  ;;; ;;;  ;;;;;;;   ;;;;;; 
;    ;;;;;   ;;; ;;;  ;;;  ;;; ;;;  ;;;          ;;; 
;     ;;;    ;;;;;;;  ;;;  ;;;;;;;   ;;;;;;  ;;  ;;; 
;     ;;;     ;;;;;;  ;;;   ;; ;;;    ;;;;    ;;;;;  
;                                                    
;                                                    
;                                                    
;                                                    
;                                                    

(with-signal (S #:combine +)
  (check-equal?
   (react!
    (esterel
     (emit S 1)))
   (hash S 1)))

(with-signal (S #:combine +)
  (check-equal?
   (react!
    (esterel
     (emit S 1)
     (emit S 2)))
   (hash S 3)))

(with-signal (S #:combine +)
  (check-exn
   #rx"signal has never been emitted"
   (λ ()
     (react!
      (esterel
       (signal-value S))))))

(with-signal (S1 #:combine + S2 #:combine +)
  (define r
    (esterel
     (emit S1 11)
     (pause)
     (emit S2 (+ 1 (signal-value S1)))))
  (check-equal? (react! r) (hash S1 11))
  (check-equal? (react! r) (hash S2 12)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-equal?
   (react!
    (esterel
     (emit S1 1)
     (emit S1 2)
     (emit S2 (+ 2 (signal-value S1)))))
   (hash S1 3 S2 5)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-equal?
   (react!
    (esterel
     (par (emit S1 1)
          (emit S2 (+ 1 (signal-value S1)))
          (emit S1 2))))
   (hash S1 3 S2 4)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (emit S1 1)
       (signal-value S1)
       (emit S1 2))))))

(with-signal (S1 #:combine + S2 #:combine +)
  (define r
    (esterel
     (emit S1 44)
     (emit S2 55)
     (pause)
     (par (if (signal-value S1)
              (void)
              (emit S2 1))
          (if (signal-value S2)
              (void)
              (void)))))
  (react! r)
  (check-equal? (react! r) (hash)))

(with-signal (S1 #:combine + S2 #:combine +)
  (define r
    (esterel
     (emit S1 #f)
     (emit S2 55)
     (pause)
     (par (if (signal-value S1)
              (void)
              (emit S2 1))
          (if (signal-value S2)
              (void)
              (void)))))
  (react! r)
  (check-equal? (react! r) (hash S2 1)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (emit S 1)
     (pause)
     (emit S 2)))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2)))

(with-signal (S #:combine + O)
  (define r
    (esterel
     #:pre 2
     (emit S 1)
     (pause)
     (when (= 1 (signal-value S #:pre 1))
       (emit O))))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O S #:combine +)
  (define r
    (esterel
     #:pre 2
     (emit S 1)
     (pause)
     (emit S 2)
     (when (= 1 (signal-value S #:pre 1))
       (emit O))
     (pause)
     (when (= 1 (signal-value S #:pre 2))
       (emit O))))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2 O #t))
  (check-equal? (react! r) (hash O #t)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (present? S)))
  (check-equal? (react! r) (hash)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (emit S 0)
     (present? S)))
  (check-equal? (react! r)
                (hash S 0)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (emit S 0)
     (present? S)
     (present? S)))
  (check-equal? (react! r)
                (hash S 0)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (present? S)))
  (check-equal? (react! r #:emit (list (cons S 0)))
                (hash S 0)))

(with-signal (S #:combine + O1 O2)
  (define r
    (esterel
     (if (present? S)
         (emit O1)
         (emit O2))))
  (check-equal? (react! r #:emit (list (cons S 2)))
                (hash S 2 O1 #t)))

(with-signal (S #:combine + O)
  (define r
    (esterel
     (let loop ()
       (when (and (present? S) (= (signal-value S) 2))
         (emit O))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r #:emit (list (cons S 1))) (hash S 1))
  (check-equal? (react! r #:emit (list (cons S 2))) (hash S 2 O #t)))

(with-signal (S #:init 0 #:combine +)
  (define r
    (esterel
     #:pre 1
     (let loop ()
       (pause)
       (emit S (+ 1 (signal-value S #:pre 1)))
       (loop))))
  (react! r)
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2))
  (check-equal? (react! r) (hash S 3)))

(with-signal (S  #:init 0 #:combine +)
  (define r
    (esterel
     #:pre 1
     (let loop ()
       (emit S (+ 1 (signal-value S #:pre 1)))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2))
  (check-equal? (react! r) (hash S 3)))

(with-signal (S #:init 0 #:combine + T #:combine +)
  (define r
    (esterel
     #:pre 1
     (let loop ()
       (emit T (signal-value S))
       (pause)
       (emit S (+ 1 (signal-value S #:pre 1)))
       (loop))))
  (check-equal? (react! r) (hash T 0))
  (check-equal? (react! r) (hash T 1 S 1))
  (check-equal? (react! r) (hash T 2 S 2)))

(with-signal (S #:init (set) #:combine set-union)
  (define r
    (esterel
     (let loop ([n 0])
       (for ([i (in-range n)]) (emit S (set i)))
       (pause)
       (loop (+ n 1)))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash S (set 0)))
  (check-equal? (react! r) (hash S (set 0 1))))


(with-signal (S #:init 0 #:combine + T U)
  (check-equal?
   (react!
    (esterel
     (par (if (present? S)
              (emit U)
              (void))
          (if (present? T)
              (void)
              (emit S 1))
          (if (present? U)
              (void)
              (void)))))
   (hash T #f S 1 U #t)))


;                                                             
;                                                             
;                                                             
;                                                             
;                                                             
;                                                             
;                                                             
;   ;;; ;;     ;;;    ;;; ;;          ;;;      ;;;    ;;; ;;  
;   ;;;;;;;   ;;;;;   ;;;;;;;        ;;;;;    ;;;;;   ;;;;;;; 
;   ;;; ;;;  ;;; ;;;  ;;; ;;;       ;;; ;;;  ;;; ;;;  ;;; ;;; 
;   ;;; ;;;  ;;; ;;;  ;;; ;;; ;;;;  ;;;      ;;; ;;;  ;;; ;;; 
;   ;;; ;;;  ;;; ;;;  ;;; ;;; ;;;;  ;;;      ;;; ;;;  ;;; ;;; 
;   ;;; ;;;  ;;; ;;;  ;;; ;;;       ;;; ;;;  ;;; ;;;  ;;; ;;; 
;   ;;; ;;;   ;;;;;   ;;; ;;;        ;;;;;    ;;;;;   ;;; ;;; 
;   ;;; ;;;    ;;;    ;;; ;;;         ;;;      ;;;    ;;; ;;; 
;                                                             
;                                                             
;                                                             
;                                                                      
;                                                                      
;              ;                             ;   ;;;                   
;            ;;;                           ;;;                         
;    ;;;;;  ;;;;;; ;;; ;;;;; ;;;    ;;;   ;;;;;; ;;; ;;;   ;;;   ;;;   
;   ;;;  ;; ;;;;;; ;;;;; ;;; ;;;   ;;;;;  ;;;;;; ;;;  ;;   ;;   ;;;;;  
;   ;;;      ;;;   ;;;   ;;; ;;;  ;;; ;;;  ;;;   ;;;  ;;; ;;;  ;;; ;;; 
;   ;;;;;;   ;;;   ;;;   ;;; ;;;  ;;;      ;;;   ;;;  ;;; ;;;  ;;; ;;; 
;    ;;;;;;  ;;;   ;;;   ;;; ;;;  ;;;      ;;;   ;;;   ;; ;;   ;;;;;;; 
;       ;;;  ;;;   ;;;   ;;; ;;;  ;;; ;;;  ;;;   ;;;   ;;;;;   ;;;     
;   ;;  ;;;  ;;;;; ;;;   ;;;;;;;   ;;;;;   ;;;;; ;;;    ;;;     ;;;;;; 
;    ;;;;;    ;;;; ;;;    ;; ;;;    ;;;     ;;;; ;;;    ;;;      ;;;;  
;                                                                      
;                                                                      
;                                                                      
;                                                                      
;                                                                      


(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (SO1 SO2)
     (define r
       (esterel
        (par
         (par
          (par
           (begin
             (pause)
             (if (present? SO1)
                 (void)
                 (begin (emit SO1)
                        (emit SO2))))
           (begin
             (pause)
             (if (present? SO2)
                 (void)
                 (begin (emit SO1)
                        (emit SO2)))))))))
     (react! r)
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (define r
       (esterel
        (if (present? S)
            (emit S)
            (void))))
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (define r
       (esterel
        (if (present? S)
            (void)
            (emit S))))
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (define r
       (esterel
        (if (present? S)
            (emit S)
            (emit S))))
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (define r
       (esterel
        (present? S)
        (emit S)))
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (react!
      (esterel
       (with-signal (S2)
         (cond
           [(present? S)
            (emit S2)]
           [else
            (if (present? S2)
                (emit S)
                (void))])))))))

(let ()
  (define r
    (esterel
     (with-signal (s)
       (suspend
        (begin (pause)
               (emit s))
        (present? s)))))

  (react! r)
  (check-exn
   non-constructive-exn?
   (λ () (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (react!
    (esterel
     (with-signal (s #:init 0 #:combine +)
       (emit s 1)
       (let ([x (signal-value s)])
         (emit s x)))))))

(check-exn
 non-constructive-exn?
 (λ ()
   (react!
    (esterel
     (with-signal (s1 s2)
       (par
        (if (present? s1)
            (emit s2)
            (void))
        (if (present? s2)
            (void)
            (emit s1))))))))

(with-signal (S1 S2 O)
  (define r
    (esterel
     (if (present? S1)
         (if (present? S2)
             (void)
             (void))
         (emit O))))
  (check-equal? (react! r)
                (hash S1 #f S2 #f O #t)))

(with-signal (S1 S2 O)
  (define r
    (esterel
     (if (present? S1)
         (void)
         (if (present? S2)
             (void)
             (emit O)))))
  (check-equal? (react! r)
                (hash S1 #f S2 #f O #t)))

(with-signal (S1 S2 S3 S4 S5)
  (define r
    (esterel
     (if (present? S1)
         (void)
         (if (present? S2)
             (if (present? S3)
                 (void)
                 (if (present? S4)
                     (if (present? S5)
                         (void)
                         (void))
                     (void)))
             (void)))))

  (check-equal? (react! r)
                (hash S1 #f S2 #f S3 #f S4 #f S4 #f S5 #f)))

(with-signal (S1 #:combine + O1 O2)
  (define r
    (esterel
     (emit S1 #f)
     (pause)
     (if (signal-value S1)
         (emit O1)
         (emit O2))))

  (react! r)
  (check-equal? (react! r)
                (hash O2 #t)))

(with-signal (S1 #:combine + O1 O2)
  (define r
    (esterel
     (emit S1 #t)
     (pause)
     (if (signal-value S1)
         (emit O1)
         (emit O2))))

  (react! r)
  (check-equal? (react! r)
                (hash O1 #t)))

(with-signal (S1 #:combine + O1 O2)
  (define r
    (esterel
     (par (emit S1 3)
          (if (signal-value S1)
              (emit O1)
              (emit O2)))))

  (check-equal? (react! r)
                (hash S1 3 O1 #t)))

(with-signal (S1 #:combine + O1 O2)
  (define r
    (esterel
     (par (emit S1 3)
          (emit S1 5)
          (if (signal-value S1)
              (emit O1)
              (emit O2)))))

  (check-equal? (react! r)
                (hash S1 8 O1 #t)))



;                                                                  
;                                                                  
;                                                                  
;                                                                  
;                                                                  
;       ;;;                                           ;;;          
;       ;;;                                                        
;    ;; ;;; ;;;   ;;; ;;; ;;    ;;;;;   ;;; ;; ;;;    ;;;    ;;;   
;   ;;;;;;;  ;;   ;;  ;;;;;;;  ;;;;;;;  ;;;;;;;;;;;   ;;;   ;;;;;  
;   ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;;  ;;; ;;; ;;;   ;;;  ;;; ;;; 
;   ;;; ;;;  ;;; ;;;  ;;; ;;;     ;;;;  ;;; ;;; ;;;   ;;;  ;;;     
;   ;;; ;;;   ;; ;;   ;;; ;;;   ;; ;;;  ;;; ;;; ;;;   ;;;  ;;;     
;   ;;; ;;;   ;;;;;   ;;; ;;;  ;;; ;;;  ;;; ;;; ;;;   ;;;  ;;; ;;; 
;   ;;;;;;;   ;;;;;   ;;; ;;;  ;;;;;;;  ;;; ;;; ;;;   ;;;   ;;;;;  
;    ;; ;;;    ;;;    ;;; ;;;   ;;;;;;  ;;; ;;; ;;;   ;;;    ;;;   
;              ;;;                                                 
;           ;;;;;                                                  
;           ;;;;;                                                  
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
;   ;;;;;;;      ;;;;     ;;;;;;;   ;;;     ;;;     ;;;    ;;;;   
;   ;;;;;;;;   ;;;;;;;;   ;;;;;;;;  ;;;     ;;;     ;;;   ;;;;;;  
;   ;;;  ;;;   ;;;  ;;;   ;;;  ;;;  ;;;     ;;;   ;;;;;   ;;; ;;; 
;   ;;;  ;;;  ;;;    ;;;  ;;;  ;;;  ;;;     ;;;  ;;;;;;   ;;; ;;; 
;   ;;;;;;;;  ;;;    ;;;  ;;;;;;;;  ;;;          ;; ;;;   ;;;;;;; 
;   ;;;;;;;   ;;;    ;;;  ;;;;;;;   ;;;             ;;;    ;; ;;; 
;   ;;;       ;;;    ;;;  ;;;       ;;;             ;;;       ;;; 
;   ;;;        ;;;  ;;;   ;;;       ;;;             ;;;   ;;; ;;; 
;   ;;;        ;;;;;;;;   ;;;       ;;;;;;;         ;;;   ;;;;;;  
;   ;;;          ;;;;     ;;;       ;;;;;;;         ;;;    ;;;;   
;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                                                 

;; popl 2019 figure 2
(with-signal (sl so1 so2)
  (check-equal?
   (react!
    (esterel
     (emit sl)
     (if (present? sl)
         (emit so1)
         (emit so2))))
   (hash sl #t so1 #t)))

;; popl 2019 figure 3
(with-signal (sl so1 so2)
  (check-equal?
   (react!
    (esterel
     (par
      (emit sl)
      (if (present? sl)
          (emit so1)
          (emit so2)))))
   (hash sl #t so1 #t)))


;; popl 2019 figure 4
(with-signal (sl so1 so2)
  (check-equal?
   (react!
    (esterel
     (if (present? sl)
         (emit so1)
         (emit so2))))
   (hash sl #f so2 #t)))

;; popl 2019 figure 5
(with-signal (sl1 sl2)
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (par
        (when (present? sl1) (emit sl2))
        (when (present? sl2) (emit sl1))))))))


;; popl 2019 figure 6
(with-signal (sl so1 so2)
  (define r
    (esterel
     (par
      (begin (pause) (emit sl))
      (if (present? sl) (emit so1) (emit so2)))))
  (check-equal?
   (react! r)
   (hash sl #f so2 #t))
  (check-equal?
   (react! r)
   (hash sl #t)))

;; popl 2019 figure 7
(with-signal (sl1 sl2 sl3 so1 so2)
  (define r
    (esterel
     (par
      (if (present? sl1)
          (if (present? sl2)
              (emit so1)
              (emit sl3))
          (if (present? sl2)
              (emit so2)
              (emit sl3)))
      (begin
        (emit sl2)
        (when (present? sl3) (pause))
        (emit sl1)))))
  (check-equal?
   (react! r)
   (hash sl1 #t sl2 #t sl3 #f so1 #t)))

;; popl 2019, figure 8
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (s1)
     (react!
      (esterel
       (if (present? s1)
           (void)
           (emit s1)))))))

;; popl 2019, figure 9
(with-signal (s1)
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (if (present? s1)
           (emit s1)
           (void)))))))


;; popl 2019, figure 10 example 1
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (s1)
     (react!
      (esterel
       (if (present? s1)
           (emit s1)
           (emit s1)))))))

;; popl 2019, figure 10 example 2
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (s1)
     (react!
      (esterel
       (if (present? s1) (void) (void))
       (emit s1))))))

;; popl 2019, figure 11
(check-exn
 exn:fail:not-constructive?
 (λ ()
   (with-signal (sl1 sl2)
     (react!
      (esterel
       (par
        (when (present? sl1) (emit sl2))
        (begin (when (present? sl2) pause)
               (emit sl1))))))))

;; popl 2019, figure 25
(with-signal (s-outer s-inner)
  (check-equal?
   (react!
    (esterel
     (begin
       (present? s-outer)
       (when (present? s-inner) (emit s-outer)))))
   (hash s-outer #f s-inner #f)))
