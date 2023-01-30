
;;;This function takes two integers and returns an integer lazy list containing the sequence of values first, first+1, ... , last.
(define seq
  (lambda (first last)
    (if (> first last)
	#f
        (cons first
              (lambda () (seq (+ first 1) last))))))

;;; This function takes an integer and returns an integer lazy list containing the infinite sequence of values
(define inf-seq
  (lambda (first)
    (cons first
	  (lambda () (inf-seq (+ first 1))))))
;;;This function takes a lazy list and an integer and returns an ordinary Scheme list containing the first n values in the lazy-list. If the lazy list contains fewer than n values, then all the values in the lazy list are returned. 
(define first-n
  (lambda (lazy-list n)
    (if (or (<= n 0) (equal? lazy-list #f))
	'()
	(cons (car lazy-list) (first-n ((cdr lazy-list)) (- n 1))))))
;;;This function takes a lazy list and an integer and returns the n-th value in the lazy-list (counting from 1). If the lazy-list contains fewer than n values, then #f is returned.
(define nth
  (lambda (lazy-list n)
    (cond ((equal? lazy-list #f)
	   #f)
	  ((equal? n 1)
	   (car lazy-list))
	  (else
	   (nth ((cdr lazy-list)) (- n 1))))))
;;;This function returns a new lazy list that has n and all integer multiples of n removed from lazy-list.
(define filter-multiples
  (lambda (lazy-list n)
    (cond ((equal? lazy-list #f)
	   #f)
	  ((equal? (modulo (car lazy-list) n) 0)
	   (filter-multiples ((cdr lazy-list)) n))
	  (else
	   (cons (car lazy-list) (lambda () (filter-multiples ((cdr lazy-list)) n)))))))


;;;computes a lazy list containing all prime numbers, using the Sieve of Eratosthenes
(define primes
  (lambda ()
    (primes-helper (inf-seq 2))))
;;Helper function to primes which actually filters the prime numbers
(define primes-helper
  (lambda (lazy-list)
    (cons (car lazy-list) (lambda () (primes-helper (filter-multiples ((cdr lazy-list)) (car lazy-list)))))))
