(define (sum-int a b)
  (if (> a b)
    0
    (+ a (sum-int (+ 1 a) b))))

(sum-int 1 100)

(define (from-to term a next b)
  (if (> a b)
    0
    (+ (term a) (from-to term (next a) next b)))) 

;1.30
(define (from-to-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc a)
  (+ 1 a))
(define (cube a)
  (* a a a))

(define (sum-cube a b)
  (from-to cube a inc b))

(define (sum-cube-iter a b)
  (from-to-iter cube a inc b))

(define (sum-integer a b)
  (from-to + a inc b))

(define (pi-sum a b)
  (define (add-4 a)
    (+ a 4))
  (define (pi-sub a)
    (/ 1.0 (* a (+ a 2))))
  (from-to pi-sub a add-4 b))

(sum-cube 1 100)
(sum-integer 1 100)
(pi-sum 1 100)

(define (integral f a b dx)
  (define (add-dx a)
    (+ a dx))
  (* (from-to f (+ a (/ dx 2.0)) add-dx b) dx))

(define (integral-cube a b)
  (integral cube a b 0.01))

(integral-cube 0 1)
(sum-cube 5 100)
(sum-cube-iter 5 100)


;1.31
(define (product func a next b)
  (if (> a b)
    1
    (* (func a) (product func (next a) next b))))
(define (new-fact n)
  (product + 1 inc n))
(new-fact 5)

(define (product-pi n)
  (define (square n)
    (* n n))
  (define (next-n n)
    (+ n 2))
  (define (func n)
    (square (/ (+ n 1) n)))
  (/ (* 2 (product func 3.0 next-n n)) n))

(* 4 (product-pi 15))

;1.32
(define (accumulate combiner null-val func a next b)
  (if (> a b)
    null-val
    (combiner (func a) (accumulate combiner null-val func (next a) next b))))

(define (accumulate-iter combiner null-val func a next b)
  (define (inner combiner func a next b place)
    (if (> a b)
      place
      (inner combiner func (next a) next b (combiner a place))))
  (inner combiner func a next b null-val))

(define (product-new func a next b)
  (accumulate * 1 func a next b))
(define (new-new-fact n)
  (product-new + 1 inc n))
(new-new-fact 5)

(define (product-new-iter func a next b)
  (accumulate * 1 func a next b))
(define (new-new-fact-iter n)
  (product-new-iter + 1 inc n))
(new-new-fact 5)

;1.33
(define (accumulate2 predicate combiner null-val func a next b)
  (cond 
    ((> a b) null-val)
    ((predicate a)(combiner (func a) (accumulate2 predicate combiner null-val func (next a) next b)))
    (else (accumulate2 predicate combiner null-val func (next a) next b))))

(define (product2 func a next b)
  (accumulate2 > * 1 func a next b))
(define (fact2 n)
  (product2 + 1 inc n))
(fact2 5)

(define (accumulate-prime a b)
  (accumulate2 even? + 0 square a (lambda (x) (+ x 1)) b))

(accumulate-prime 1 100)

;1.3.2
(define (pi-sum a b)
  (from-to 
    (lambda (x) (/ 1.0 (* x (+ x 2)))) 
    a 
    (lambda (x) (+ x 4))
    b))
(pi-sum 1 40)

((lambda (x y z) (+ x y z)) 1 2 3)

(define (func-f x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(func-f 5 5)

(+ (let ((x 3))
     (+ x (* x 10))) 5)

;1.34
;(define (f g)
;  (g 2))
;error
;
;1.3.3

(define (average a b)
  (/ (+ a b) 2.0))
(define (close-enough? a b)
  (< (abs (- a b)) 0.0001))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      mid-point
      (let ((test-value (f mid-point)))
	(cond 
	  ((> test-value 0) (search f neg-point mid-point))
	  ((< test-value 0) (search f mid-point pos-point))
	  (else mid-point))))))
(define (half-interval-method f a b)
  (let ((f-a (f a))
	(f-b (f b)))
    (cond 
      ((and (> f-a 0) (< f-b 0)) (search f b a))
      ((and (< f-a 0) (> f-b 0)) (search f a b))
      (else (error "values not right" a b)))))

(half-interval-method (lambda (x) (- 1 (cube x))) -10 10)
(half-interval-method sin 2 4)
(half-interval-method (lambda (x) (- (cube x) (* 2 x) 3)) -10 10)

(define (fixed-point f first-guess)
  (let ((next-val (f first-guess)))
    (if (close-enough? first-guess next-val)
      first-guess
      (fixed-point f next-val))))

(fixed-point cos 1.0)
(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)

(define (sqrt2 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0)) 

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;1.36
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

;1.37
(define (cont-frac-iter n d k)
  (define (inner n d k place)
    (cond
      ((= k 1) place)
      (else (inner n d (- k 1) (/ (n k) (+ (d k) place))))))
  (inner n d k 0.0))

(cont-frac-iter 1.0 1.0 100)

(define (cont-frac n d k)
  (if (= k 0)
    (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 100)

(cont-frac (lambda (x) 1.0) 
	   (lambda (x) 
	     (if (= (remainder (+ 1 x) 3) 0) 
	       x
	       1.0)) 100)

;1.39
(define (tan-cf x k)
  (- (cont-frac (lambda (k) (- (expt x k)))
	     (lambda (x) (- (* 2.0 k) 1.0))
	     k)))
(tan-cf 180 100)

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10.0) 
(define (sqrt3 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))
(sqrt3 9)

