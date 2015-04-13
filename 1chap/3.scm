(define (sum-int a b)
  (if (> a b)
    0
    (+ a (sum-int (+ 1 a) b))))

(sum-int 1 100)

(define (from-to term a next b)
  (if (> a b)
    0
    (+ (term a) (from-to term (next a) next b)))) 

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
