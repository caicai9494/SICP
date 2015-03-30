(define (my-factorial x)
  (if (= x 1) 
    1
    (* x (my-factorial (- x 1)))
    )
  )
(my-factorial 5)

(define (my-factorial-iter max-count)
  (define (my-fact-iter max-count product count)
    (if (> count max-count)
      product
      (my-fact-iter max-count (* product count) (+ 1 count))
      )
    )
  (my-fact-iter max-count 1 1)
  )

(my-factorial-iter 5)

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(sqrt 65536)

(define (fib count)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))
      )
  )
  (fib-iter 1 0 count)
  )

(fib 5)

(define (count-change amount)
  (define (first-denomination kind-of-coin)
    (cond
      ((= kind-of-coin 1) 1)
      ((= kind-of-coin 2) 5)
      ((= kind-of-coin 3) 10)
      ((= kind-of-coin 4) 25)
      ((= kind-of-coin 5) 50)))

  (define (cc amount kind-of-coin)
    (cond 
      ((= amount 0) 1)
      ((or (< amount 0) (= kind-of-coin 0)) 0)
      (else (+ (cc amount 
		   (- kind-of-coin 1))
	       (cc (- amount (first-denomination kind-of-coin))
		   kind-of-coin)))))

  (cc amount 5)
)
(count-change 100)

(define (fn n)
  (if (< n 3)
    n
    (+ (fn (- n 1)) (* 2 (fn (- n 2))) (* 3 (fn (- n 3))))
    ))

(fn 1)
(fn 14)

(define (fn-2 n)
    (define (fn-iter a b c n)
      (cond
	((= n 3) c)
        ((< n 3) n)
	(else
	  (fn-iter b c (+ (* 3 a) (* 2 b) c) (- n 1))
	))
    )
    (fn-iter 1 2 3 n))
(fn-2 10)

(define (pascal height pos)
  (cond 
    ((or (= pos 1) (= pos height)) 1)
    ((> pos height) 0)
    (else
      (+ (pascal (- height 1) (- pos 1)) (pascal (- height 1) pos))
      )))

(pascal 40 25)

