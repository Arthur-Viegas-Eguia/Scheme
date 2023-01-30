;;Adds up the elements in a list
(define sum
  (lambda (lst)
    (if (equal? (length lst) 0)
	0 
	(+ (car lst) (sum (cdr lst)))))) ;;Otherwhise, call the function recursively


;;returns a list of the first n elements of the list lst
(define keep-first-n
  (lambda (n lst)
    (cond ((equal? n 0) (list)) 
	  ((> n (length lst)) (list)) 
	  ((< n 0) (list)) 
	  (else (cons (car lst) (keep-first-n (- n 1) (cdr lst))))))) 
