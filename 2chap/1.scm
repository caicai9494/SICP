(define (make-rat n d)
  (cons n d))

(define (numer x)
  (car x))

(define (demon x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (demon x)))

(print-rat (make-rat 2 3))

(define (gcd a b)
  (if (= b 0) 
    a
    (gcd b (remainder a b))))



;2.1
(define (make-rat2 n d)
  (let ((g (gcd n d))
	(gn (/ n (gcd n d)))
	(gd (/ d (gcd n d))))
    (if (< (* gn gd) 0)
      (make-rat (- (abs gn)) (abs gd))
      (make-rat gn gd))))

(print-rat (make-rat2 2 -4))

;2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (mid-point p1 p2)
  (define (add-divide a b)
    (/ (+ a b) 2.0))
  (make-point (add-divide (x-point p1) (x-point p2))
	      (add-divide (y-point p1) (y-point p2))))
(define (dist p1 p2)
  (define (square x) (* x x))
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
	   (square (- (y-point p1) (y-point p2))))))



(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-point s)
  (car s))
(define (end-point s)
  (cdr s))
(define (midpoint-seg s)
  (mid-point (start-point s) (end-point s)))
(define (seg-dist s)
  (dist (start-point s) (end-point s)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(y-point (make-point 3 4))
(print-point (make-point 3 4))
(print-point (midpoint-seg (make-segment (make-point 3 4) (make-point 5 2))))
(seg-dist (make-segment (make-point 3 4) (make-point 5 2)))


;2.3
(define (make-rect p1 p2)
  (cons p1 p2))

(define (cons2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "invalid argument"))))
  dispatch)

(define (car2 z) (z 0))
(define (cdr2 z) (z 1))

(car2 (cons2 3 4))

(define (cons3 x y)
  (lambda (m) (m x y)))

(define (car3 z)
  (z (lambda (p q) p)))
(define (cdr3 z)
  (z (lambda (p q) q)))
(car3 (cons3 3 4))
(cdr3 (cons3 3 4))

;2.5
(define (logB x b)
  (/ (log x) (log b)))

(define (cons4 a b)
  (* (expt 2 a) (expt 3 b)))
(define (car4 z)
  (let ((div3 (/ z 3)))
    (if (= 0 (modulo div3 3))
      (car4 div3)
      (logB div3 2))))

(define (cdr4 z)
  (let ((div2 (/ z 2)))
    (if (= 0 (modulo div2 2))
      (cdr4 div2)
      (logB div2 3))))

(car4 (cons4 3 4))
(cdr4 (cons4 3 4))

;2.6
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (f) (lambda (x) x))))
(define two (lambda (f) (lambda (f) (lambda (f) (lambda (x) x)))))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(add-1 one)
(add-1 two)

;2.7
(define (make-interval l u)
  (if (< l u)
    (cons l u)
    (cons u l)))

(define (lowerbound x)
  (car x))
(define (upperbound x)
  (cdr x))
(define (add-interval x y)
  (make-interval (+ (lowerbound x) (lowerbound y))
		 (+ (upperbound x) (upperbound y))))
(define (mult-interval x y)
  (let ((p1 (* (lowerbound x) (lowerbound y)))
	(p2 (* (upperbound x) (upperbound y)))
	(p3 (* (lowerbound x) (upperbound y)))
	(p4 (* (upperbound x) (lowerbound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(define (div-interval x y)
  (define (span-zero z) (and (<= (lowerbound z) 0) (>= (upperbound z) 0)))
  (if (and (span-zero x) (span-zero y))
    (mult-interval x (make-interval (/ 1.0 (upperbound y))
				  (/ 1.0 (lowerbound y))))
    (error "span zero")))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upperbound y))
				  (- (lowerbound y)))))
(define (width-interval x)
  (- (upperbound x) (lowerbound x)))

(define (print-interval x)
  (newline)
  (display "[")
  (display (lowerbound x))
  (display ", ")
  (display (upperbound x))
  (display "]"))

(define itv1 (make-interval -3 -5))
(define itv2 (make-interval -8 -4))
(print-interval itv1)
(print-interval itv2)
(print-interval (add-interval itv1 itv2))
(print-interval (mult-interval itv1 itv2))
(print-interval (div-interval itv1 itv2))
(print-interval (sub-interval itv1 itv2))

(define (make-interval2 c r)
  (let ((spread (* c r)))
    (make-interval (- c spread) (+ c spread))))
(define (center z)
  (/ (+ (car z) (cdr z)) 2))
(define (ratio z)
  (/ (- (cdr z) (car z)) (* 2 (center z))))

(define itv3 (make-interval2 8 .1))
(print-interval itv3)
(center itv3)
(ratio itv3)

(define nil ())
(cons 1 (cons 2 (cons 3 (cons 4 nil))))
(define one-through-four (list 1 2 3 4))
(define odds (list 1 3 5 7))
one-through-four

(define (list-ref2 l n)
  (if (= n 0)
    (car l)
    (list-ref2 (cdr l) (- n 1))))
(define (length2 l)
  (if (not (null? l))
    (+ 1 (length2 (cdr l)))
    0))
(define (append2 lhs rhs)
  (if (null? rhs)
   lhs
    (append2 (cons lhs (car rhs)) (cdr rhs))))

(define (append3 lhs rhs)
  (if (null? lhs)
    rhs
    (cons (car lhs) (append3 (cdr lhs) rhs))))


(list-ref2 one-through-four 2)
(length one-through-four)
(length2 one-through-four)
(append one-through-four odds)
(append2 one-through-four odds)
(append3 one-through-four odds)

;2.17
(define (last-pair l)
  (if (null? (cdr l))
    (car l)
    (last-pair (cdr l))))
(last-pair one-through-four)

(append (cdr one-through-four) (car one-through-four))

;2.18
(define (reverse2 l)
  (if (null? (cdr l))
    l
    (append2 (reverse2 (cdr l))
	     (list (car l)))))


(reverse2 one-through-four)

(+ one-through-four)
  
(list nil 3)

;2.19
(define us-coin (list 50 25 10 5 1))
(define us-coin2 (list 1 5 10 25 50))
(define uk-coin (list 100 50 20 10 5 2 1 .5))

(define (cc amount coin-values)

  (define no-more? null?)

  (define (first-domination coin-values) 
    (car coin-values))
	   
  (define (except-first-domination coin-values) 
    (cdr coin-values))

  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else 
	  (+ (cc amount (except-first-domination coin-values))
	     (cc (- amount (first-domination coin-values)) coin-values)))))

(cc 100 us-coin)

;2.20
(define (same-parity . z)
  (define (same-parity-sub source dist count)
    (if (null? source) 
      dist
      (if (even? count) 
	(same-parity-sub (cdr source) (cons (car source) dist) (+ 1 count))
	(same-parity-sub (cdr source) dist (+ 1 count)))))
  (same-parity-sub z nil 0))

(same-parity 1 2 3 4 5)

