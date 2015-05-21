(define (scale-list items factor)
  (if (null? items)
    nil
    (cons (* factor (car items)) (scale-list (cdr items) factor))))
(scale-list one-through-four 2)

(define (map2 items proc)
  (if (null? items)
    nil
    (cons (proc (car items)) (map2 (cdr items) proc))))
(map2 one-through-four (lambda (x) (* x x)))
(map2 one-through-four (lambda (x) (* 2 x)))

(define (reduce items prop)
  (if (null? items)
    nil
    (if (prop (car items))
      (cons (car items) (reduce (cdr items) prop)))))
(reduce one-through-four even?)

;2.21
(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
    nil
    (cons (square (car items)) (square-list (cdr items)))))
(square-list one-through-four)
  
  
(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
	    (cons answer(square (car things))))))
  (iter items nil))

(square-list one-through-four)

;2.23
(define (foreach action items)
  (cond ((null? items) #t)
	(else (action (car items))
	      (foreach action (cdr items)))))

(foreach (lambda (x) (newline) (display x)) (list 2 3 4))

(define example1 (cons (list 1 2) (list 3 4)))
(length example1)

(define (count-leaves items)
  (cond ((null? items) 0)
	((not (pair? items)) 1)
	(else (+ (count-leaves (car items))
		 (count-leaves (cdr items))))))
(count-leaves example1)

;2.24
(define example2 (list 1 (list 2 (list 3 4))))
example2
(length example2)
(count-leaves example2)

;2.25
(define example3 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr example3)))))
(define example4 (list (list 7)))
(car (car example4))
(define example5 (list 1 (list 2 (list 3 (list 4 (list 5))))))
(car (cdr (car (cdr (car (cdr example5))))))

;2.26
(define examplex (list 1 2 3))
(define exampley (list 4 5 6))
(append examplex exampley)
(cons examplex exampley)
(list examplex exampley)

;2.27
