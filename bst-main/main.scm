;;Checks the element passed is a BST
(define check-bst
    (lambda (bst)
    (cond ((null? bst) #t)
      ((and (list? bst) (and (equal? 3 (length bst)) (and (list? (car (cdr bst))) (list? (car (cdr (cdr bst))))))) #t)
      (else #f)
      )))

;; returning the root of bst
(define entry
  (lambda (bst)
    ;; checking if the root is a number and if the element passed is a bst
    (cond ((< (length bst) 3) #f)
          ((integer? (car bst)) (car bst))
          (else #f)
    )))
;; Returns the left subtree
(define left
  (lambda (bst)
    ;; checking if the root is a number and if the element passed is a bst
    (cond ((< (length bst) 3) #f)
          ((and (integer? (entry bst)) (and (list? (car (cdr (cdr bst)))) (list? (car (cdr bst))))) (car (cdr bst)))
          (else #f)
    )))

;; Returns the right subtree
(define right
  (lambda (bst)
    ;; checking if the root is a number and if the element passed is a bst
    (cond ((< (length bst) 3) #f)
          ((and (integer? (entry bst)) (and (list? (car (cdr bst))) (list? (car (cdr (cdr bst)))))) (car (cdr (cdr bst))))
          (else #f)
    )))

;;Creates a bst from the information provided
(define make-bst 
    (lambda (elt left-bst right-bst)
        (if (and (integer? elt) (and (check-bst left-bst) (check-bst right-bst)))
            (list elt left-bst right-bst)
            #f    
        )
    ))

;;Return a list containing all values in bst in the order obtained from a preorder traversal
(define preorder
    (lambda (bst)
        (if (null? bst) 
            '()
            (cons (car bst) (append (preorder (car (cdr bst))) (preorder (car (cdr (cdr bst))))))
        )))

;;Return a list containing all values in bst in the order obtained from an inorder traversal
(define inorder
    (lambda (bst)
        (if (null? bst) 
            '()
            (append (inorder (car (cdr bst))) (cons (car bst) (inorder (car (cdr (cdr bst))))))
        )))

;;Return a list containing all values in bst in the order obtained from an postorder traversal
(define postorder
    (lambda (bst)
        (if (null? bst) 
            '()
            (append (postorder (car (cdr bst))) (postorder (car (cdr (cdr bst)))) (cons (car bst) `()))
        )))

;; Returns a new binary search tree identical to bst but with integer v appearing in its proper location.
(define insert
    (lambda (v bst)
        (cond ((null? bst) (make-bst v '() '()))
        ((> (entry bst) v) (make-bst (entry bst) (insert v (left bst)) (right bst)))
        ((< (entry bst) v) (make-bst (entry bst) (left bst) (insert v (right bst))))
        (else bst)
        )))





