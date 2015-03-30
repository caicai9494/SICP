(+ 1 2 3)
(cons 3 (cons 4 (cons 5 6))) 
(cons 0 ())
(cons "hello" "world")
(cons 1 (cons 2 (cons 3 ())))
(cons "I" (cons "saw" (cons 3 (cons "girl" ()))))
(cons "sum of"  (cons (cons 1 (cons 2 (cons 3 (cons 4 ())))) (cons "is" (cons "10" ())))) 
'(1 2 3)
(cdr '(0))
(define vhello "hello")
vhello
(define fhello (lambda() 
		 "hello")
  )

(define hello (lambda(name)
		(string-append "hello" name "!")
		)
  )
(define sum3 (lambda(n1 n2 n3)
	       (+ n1 n2 n3)
	       )
  )

(hello "yes")
(sum3 1 2 3)

(define (addone n)
  (+ n 1)
  )

(addone 5)

(define PI (* 4 (atan 1.0)))
(* 1 PI)

(null? '(a b))

(define (sum-gp n)
  (if (= n 0)
    n 
    (/ 1 n)
    )
  )

(sum-gp 5)
(sum-gp 0)

(define (absolute n)
  (if (> n 0)
    n
    (if (= n 0)
      0
      (- n)
      )
    )
  )

(absolute 0)

(define (grade g)
  (cond
    ((>= 80) "A")
    ((<= 60 g 79) "B")
    ((<= 40 g 59) "C")
    )
  )


(define mysquare (lambda (x) (* x x)))

(mysquare 5)

(define (good-enough? a b)
  (< (absolute (- (mysquare a) b)) 0.001))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (mysqrt guess x)
  (if (good-enough? guess x)
	guess
	(mysqrt (improve guess x) x)))
(define (sqrt-iter x)
  (mysqrt 1 x))

(sqrt-iter 5.0)

(sqrt 5.0)

(if (> 5 3)
  (if (> 5 4) 5 4)
  3)
(= 2 3)
(cond 
  ((> 4 5) 4 )
  (else 3)
  )

(/ (+ (+ 5 4) (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7)))

(define (two-larger-square-sum a b c)
  (define (square-sum x y)
    (+ (square x) (square y))
    )
  (cond 
    ((and (> a c) (> b c)) (square-sum a b))
    ((and (> a b) (> c b)) (square-sum a c))
    ((and (> b a) (> c a)) (square-sum b c))
    )
  )

(two-larger-square-sum 1 2 3)
(two-larger-square-sum 5 2 3)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)
  )
(a-plus-abs-b 1 -2)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)
	)
  )

(new-if (> 1 0) 1 0)

(define (sqrt-iter2 x)
    (define (mysqrt2 guess x)
      (new-if (good-enough? guess x)
	    guess
	    (mysqrt2 (improve guess x) x))
          )
      (mysqrt2 1 x)
    )

(define (cube-root x)
  (define (improve-cube guess)
    (/ (+ (/ x  (* guess guess)) (* 2 guess)) 3)
    )
  (define (good-enough?? guess last-guess)
    (< (abs (- guess last-guess)) 1e-6)
    )

  (define (cube-iter guess)
    (if (good-enough?? guess (improve-cube guess))
      guess
      (cube-iter (improve-cube guess))
      )
    )
  (cube-iter 1.0)
  )

(cube-root 8)
