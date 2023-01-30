;; takes a three-argument function and returns a curried version of that function
(define curry3
  (lambda (f)
    (lambda (x)
      (lambda (y)
	(lambda (z)
	  (f x y z))))))

;; takes a curried three-argument function and returns a normal Scheme uncurried version of that function.
(define uncurry3
  (lambda (f)
    (lambda (x y z)
      (((f x) y) z))))

;; helper function for the general uncurrying function
(define uncurry-helper
  (lambda (f lst)
    (if (equal? (length lst) 1)
	(f (car lst))
	(uncurry-helper (f (car lst)) (cdr lst)))))

;; takes a curried function of any number of parameters and returns a normal Scheme uncurried version of that function
(define uncurry
  (lambda (f)
    (lambda args
       (uncurry-helper f args))))

;; applies procedure to each element of list and returns a new list containing only the elements for which procedure returns true
(define my-filter
  (lambda (f lst)
    (cond ((null? lst) '())
	  ((f (car lst)) (cons (car lst) (my-filter f (cdr lst))))
	  (else (my-filter f (cdr lst))))))

;; helper function for handling list comparison, which returns true if a is not equal to b
(define not-equal
  (lambda (a)
    (lambda (b)
    (if (not (equal? a b))
	#t
	#f))))


;; helper function for handling list comparison, which returns true if a equals to b
(define equal
  (lambda (a)
    (lambda (b)
    (if (equal? a b)
	#t
	#f))))


;; handles set union
(define union
  (lambda (lst1 lst2)
    (if (null? lst1)
	lst2
	(cons (car lst1) (union (cdr lst1) (my-filter (not-equal (car lst1)) lst2))))))

;; handles set intersection
(define intersect
  (lambda (lst1 lst2)
    (cond ((null? lst1) '())
	  ((null? (my-filter (equal (car lst1)) lst2)) (intersect (cdr lst1) lst2))
	  (else (cons (car lst1) (intersect (cdr lst1) lst2))))))


(define exists
  (lambda (f lst)
    (if (null? (my-filter f lst))
	#f
	#t)))
(define plus (lambda (a b c)
	       (+ a (+ b c))))
