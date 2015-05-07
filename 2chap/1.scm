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
(cons 1 (cons 2 3))


