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
		

