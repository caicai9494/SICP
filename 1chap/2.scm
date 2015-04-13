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

(pascal 40 1)

(define (sine ang)
  (define (cube x)
    (* x x x))
  (define (p x)
    (- (* 3 x) (* 4 (cube x))))
  (if (< ang 0.01)
    ang
    (p (sine(/ ang 3.0)))))

(sine 1)

(define (fast-pow  base power)
  (cond
    ((= power 0) 1)
    ;((= power 1) base)
    ((even? power) (fast-pow (* base base) (/ power 2)))
    (else (* base (fast-pow base (- power 1))))
    ))
(fast-pow 2 10)

(define (fast-mul lhs rhs)
  (cond 
    ((or (= lhs 0) (= rhs 0)) 0)
    ((even? rhs) (fast-mul (* 2 lhs) (/ rhs 2)))
    (else (+ lhs (fast-mul lhs (- rhs 1))))))

(fast-mul 5 40)

(define (mul-iter lhs rhs)
  (cond 
    ((or (= lhs 0) (= rhs 0)) 0)
    (else (+ lhs (mul-iter lhs (- rhs 1))))))

(mul-iter 5 40)

(define (fast-mul-i lhs rhs)
(define (fast-mul-iter lhs rhs holder)
  (cond 
    ((= rhs 1) holder)
    ((or (= lhs 0) (= rhs 0)) 0)
    ((even? rhs) (fast-mul-iter lhs (/ rhs 2) (* 2 lhs)))
    (else (fast-mul-iter lhs (- rhs 1) (+ lhs holder)))))
  (fast-mul-iter lhs rhs 0)
)

(fast-mul-i 5 20)


(define (prime? n)
  (define (prime-inner n testn)
    (cond 
      ((or (= n 2) (> testn (sqrt n))) #t)
      ((= 0 (remainder n testn)) #f)
      (else (prime-inner n (+ 1 testn)))))
  (prime-inner n 2))

(prime? 3)
(prime? 4)
(prime? 17)
(prime? 18)

(define (gcd a b)
  (if (= b 0) 
    a
    (gcd b (remainder a b))))
(gcd 206 40)

(define (expmod base power rmd)
  (cond
    ((= power 0) 1)
    ((even? power) 
     (remainder (expmod (* base base) (/ power 2) rmd) rmd))
    (else (remainder (* base (expmod base (- power 1) rmd)) rmd))
    ))

(expmod 2 5 4)

(define (fermat-test n)
  (define (try-it r)
    (= (expmod r n n) r))
  (try-it (+ 1 (random (- n 1)))))

(define (quick-prime? n)
  (define (inner n count)
    (cond
      ((= count 0) #t)
      ((fermat-test n) (inner n (- count 1)))
      (else #f)))
  (inner n 1)
  )

(quick-prime? 100)

(gcd 19999 1999)

(define (timed-prime-test n)
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
    )
  (define (start-prime-test n start-time)
    (if (quick-prime? n)
      (report-prime (- (runtime) start-time))))
  (newline)
  (display n)
  (start-prime-test n (runtime))
  )

(timed-prime-test 53)

(define (prime_start-from n count)
  (define (time-prime-start-from n count start-time)
    (cond
      ((= count 0) (- (runtime) start-time))
      ((quick-prime? n) (newline) (display n) (time-prime-start-from (+ 2 n) (- count 1) start-time)) 
      (else (time-prime-start-from (+ 2 n) count start-time))))
  (time-prime-start-from n count (runtime)))


(prime_start-from 1001 3)
;(prime_start-from 10001 3)
;(prime_start-from 100001 3)

(define (small-divisor n)
  (define (find-divisor n test-n)
    (cond 
      ((> (* test-n test-n) n) n)
      ((= (remainder n test-n) 0) test-n)
      (else (find-divisor n (+ 1 test-n)))))
  (find-divisor n 2)
  )

(define (small-divisor-rev n)
  (define (next n) (+ n 2))
  (define (find-divisor n test-n)
    (cond 
      ((> (* test-n test-n) n) n)
      ((= (remainder n test-n) 0) test-n)
      (else (find-divisor n (next test-n)))))
  (find-divisor n 2)
  )

(define (timing func para)
  (define (timing-inner func start-time)
    (func para)
    (newline)
    (display (- (current-inexact-milliseconds) start-time)))
  (timing-inner func (current-inexact-milliseconds)))


(define (quick-prime2? n)
  (define (inner n count)
    (cond
      ((= count 0) #t)
      ((fermat-test n) (inner n (- count 1)))
      (else #f)))
  (inner n n))

(quick-prime2? 561)
