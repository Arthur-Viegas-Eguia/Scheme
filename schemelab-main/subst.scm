;; Returns a new binary search tree identical to bst but with integer v appearing in its proper location.
(define insert
    (lambda (v bst)
      (cond ((null? bst) (make-tree  v '() ()))
	    ((> (car bst) v) (append bst ))
        )))
