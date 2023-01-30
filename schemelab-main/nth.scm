(define nth
  (lambda (lst n)
    (cond [(> n (length lst)) '()]
	  [(<= n 0) '()]
	  [(equal? n 1) (car lst)]
	  [else (nth (cdr lst) (- n 1))]
	  
     )))


,tr (nth '(1 2 3 4 5 6) 0)

