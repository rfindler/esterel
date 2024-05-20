#lang racket/base

(require esterel/full rackunit)

(define-signal A B C x y z)

(test-begin (define e (esterel
                       (await #:cases
                              [(present? A) (emit x)]
                              [(present? B)]
                              [#:immediate (present? C) (emit z)])))
            (define first-instant-result (react! e))
            (define second-instant-result (react! e))
            (define third-instant-result (react! e))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) #f)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref third-instant-result C 'not-in-hash) #f)
            (check equal? (hash-ref third-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref third-instant-result B 'not-in-hash) #f)
            (check equal? (hash-ref third-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result z 'not-in-hash) 'not-in-hash))

(test-begin (define e (esterel
                       (await #:cases
                              [(present? A) (emit x)]
                              [(present? B)]
                              [#:immediate (present? C) (emit z)])))
            (define first-instant-result (react! e #:emit (list B)))
            (define second-instant-result (react! e #:emit (list A B C)))
            (define third-instant-result (react! e))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) #f)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref third-instant-result C 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result A 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result z 'not-in-hash) 'not-in-hash))

(test-begin (define e (esterel
                       (await #:cases
                              [(present? A) (emit x)]
                              [(present? B)]
                              [#:immediate (present? C) (emit z)])))
            (define first-instant-result (react! e #:emit (list B)))
            (define second-instant-result (react! e #:emit (list B C)))
            (define third-instant-result (react! e))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) #f)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref third-instant-result C 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result A 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result z 'not-in-hash) 'not-in-hash))

(test-begin (define e (esterel
                       (await #:cases
                              [(present? A) (emit x)]
                              [(present? B)]
                              [#:immediate (present? C) (emit z)])))
            (define first-instant-result (react! e #:emit (list B)))
            (define second-instant-result (react! e #:emit (list)))
            (define third-instant-result (react! e #:emit (list B C)))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) #f)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref third-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref third-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref third-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref third-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result z 'not-in-hash) 'not-in-hash))

(test-begin (define e (esterel
                       (await #:cases
                              [(present? A) (emit x)]
                              [(present? B)]
                              [#:immediate (present? C) (emit z)])))
            (define first-instant-result (react! e #:emit (list C)))
            (define second-instant-result (react! e #:emit (list)))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) #t)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash))

(test-begin (define e (esterel
                       (await #:cases
                              [#:immediate (present? A) (emit x)]
                              [(present? B)]
                              [#:immediate (present? C) (emit z)])))
            (define first-instant-result (react! e #:emit (list A C)))
            (define second-instant-result (react! e #:emit (list A B C)))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash))

(test-begin (define e (esterel
                       (await #:cases
                              [(present? A) (emit x)]
                              [(present? B) (emit y)]
                              [(present? C) (emit z)])))
            (define first-instant-result (react! e #:emit (list A B C)))
            (define second-instant-result (react! e #:emit (list B C)))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) #t)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash))

(test-begin (define e (esterel
                       (await #:cases
                              [#:immediate (present? A) (emit x)]
                              [(present? B) (emit y)]
                              [(present? C) (emit z)])))
            (define first-instant-result (react! e #:emit (list)))
            (define second-instant-result (react! e #:emit (list)))
            (define third-instant-result (react! e #:emit (list A C)))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref third-instant-result C 'not-in-hash) #t)
            (check equal? (hash-ref third-instant-result A 'not-in-hash) #t)
            (check equal? (hash-ref third-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result x 'not-in-hash) #t)
            (check equal? (hash-ref third-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result z 'not-in-hash) 'not-in-hash))

(test-begin (define e (esterel
                       (await #:cases
                              [(present? A) (emit x)])))
            (define first-instant-result (react! e #:emit (list A)))
            (define second-instant-result (react! e #:emit (list)))
            (define third-instant-result (react! e #:emit (list A)))

            (check equal? (hash-ref first-instant-result C 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result A 'not-in-hash) #t)
            (check equal? (hash-ref first-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref first-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref second-instant-result C 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result A 'not-in-hash) #f)
            (check equal? (hash-ref second-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result x 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref second-instant-result z 'not-in-hash) 'not-in-hash)

            (check equal? (hash-ref third-instant-result C 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result A 'not-in-hash) #t)
            (check equal? (hash-ref third-instant-result B 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result x 'not-in-hash) #t)
            (check equal? (hash-ref third-instant-result y 'not-in-hash) 'not-in-hash)
            (check equal? (hash-ref third-instant-result z 'not-in-hash) 'not-in-hash))
