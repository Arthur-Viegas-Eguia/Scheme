;;;Generates a list of consecutive integers, from start to stop. (If start > stop then the empty list is generated.)
(define gen-list
  (lambda (start stop)
    (if (> start stop)
	'()
	(cons start (gen-list (+ start 1) stop)))))

;;;Tests whether any two adjacent values in a list sum to val.
(define pair-sum?
  (lambda (lst val)
    (cond ((or (null? lst) (null? (cdr lst))) #f)
	  ((equal? (+ (car lst) (car (cdr lst))) val) #t)
	  (else (pair-sum? (cdr lst) val)))))

;;; Defines a pair of values. The car is the first integer in the sequence. The cdr is a function that, when called, will return another pair
(define gen-lazy-list
  (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
              (lambda () (gen-lazy-list (+ start 1) stop))))))


;;;Tests whether any two adjacent values in a lazy list sum to val.
(define pair-sum-lazy?
  (lambda (lst val)
    (cond ((or (null? lst) (or (equal? lst #f) (equal? ((cdr lst)) #f)))
	   #f)
	  ((equal? (+ (car lst) (car ((cdr lst)))) val)
	   #t)
	  (else
	   (pair-sum-lazy? ((cdr lst)) val)))))

;;Gets a scheme list as a parameter, and returns a lazy version of that list
(define make-lazy
  (lambda (lst)
   (if (null? lst)
        #f
        (cons (car lst)
              (lambda () (make-lazy (cdr lst)))))))
