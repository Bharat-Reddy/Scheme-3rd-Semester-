(define make-Emptybst '())
(define (make-bst bstVal bstLs bstRs) (list bstVal bstLs bstRs))
(define (get-bstVal bst) (car bst))
(define (get-bstLs bst) (cadr bst))
(define (get-bstRs bst) (caddr bst))
(define (isEmptybst? bst) (null? bst))
(define (isLeaf? bst) 
 (and (isEmptybst? (get-bstLs bst))
      (isEmptybst? (get-bstRs bst))))

(define (isMemberbst? bstVal bst)
 (cond ((isEmptybst? bst) #f)
       ((= bstVal (get-bstVal bst)) #t)
       ((< bstVal (get-bstVal bst)) (isMemberbst? bstVal (get-bstLs bst)))
       (else (isMemberbst? bstVal (get-bstRs bst)))))

(define (get-Sizebst bst)
 (cond ((isEmptybst? bst) 0)
       (else (+ 1 (get-Sizebst (get-bstLs bst))
	         (get-Sizebst (get-bstRs bst))))))

(define (bst-insert bstVal bst)
 (cond ((isEmptybst? bst) (make-bst bstVal make-Emptybst make-Emptybst))
       ((= bstVal (get-bstVal bst)) bst)
       ((< bstVal (get-bstVal bst)) (make-bst (get-bstVal bst) (bst-insert bstVal (get-bstLs bst)) (get-bstRs bst)))
       (else (make-bst (get-bstVal bst) (get-bstLs bst) (bst-insert bstVal (get-bstRs bst))))))

(define (bst-sort bst)
 (if (isEmptybst? bst)
     make-Emptybst
     (append (bst-sort (get-bstLs bst)) (list (get-bstVal bst)) (bst-sort (get-bstRs bst)))))

(define (preOrder-Traversal bst)
 (if (isEmptybst? bst)
     '()
     (append (list (get-bstVal bst)) (preOrder-Traversal (get-bstLs bst)) (preOrder-Traversal (get-bstRs bst)))))

(define (postOrder-Traversal bst)
 (if (isEmptybst? bst)
     make-Emptybst
     (append (postOrder-Traversal (get-bstLs bst)) (postOrder-Traversal (get-bstRs bst)) (list (get-bstVal bst))))) 

(define (get-Maxbst bst)
 (cond ((isEmptybst? (get-bstRs bst)) (get-bstVal bst))
       (else (get-Maxbst (get-bstRs bst))))) 

(define (isEqual bst1 bst2)
 (cond ((and (isEmptybst? bst1) (isEmptybst? bst2)) #t)
       ((or (isEmptybst? bst1) (isEmptybst? bst2)) #f)
       ((not (= (get-bstVal bst1) (get-bstVal bst2))) #f)
       ((= (get-bstVal bst1) (get-bstVal bst2)) (and (isEqual (get-bstLs bst1) (get-bstLs bst2))
	                                             (isEqual (get-bstRs bst1) (get-bstRs bst2))))))
(define (max-Height bst)
 (cond ((isEmptybst? bst) 0)
       (else (max (+ 1 (max-Height (get-bstLs bst)))
	          (+ 1 (max-Height (get-bstRs bst)))))))

(define (hasPathSum bstVal bst)
 (cond ((isEmptybst? bst) #f)
       ((= (- bstVal (get-bstVal bst)) 0) (if (isLeaf? bst) #t #f))
       (else (or (hasPathSum (- bstVal (get-bstVal bst)) (get-bstLs bst))
	         (hasPathSum (- bstVal (get-bstVal bst)) (get-bstRs bst))))))


(define (get-CommonAnsestor b1 b2 bst)
 (cond ((isEmptybst? bst) "Element not Found")
       ((and (< b1 (get-bstVal bst))
	     (< b2 (get-bstVal bst))) (get-CommonAnsestor b1 b2 (get-bstLs bst)))
       ((and (> b1 (get-bstVal bst))
	     (> b2 (get-bstVal bst))) (get-CommonAnsestor b1 b2 (get-bstRs bst)))
       (else (get-bstVal bst))))


(define (mirror bst)
 (cond ((isEmptybst? ) make-Emptybst)
       (else (make-bst (get-bstVal bst)
	               (mirror (get-bstRs bst))
		       (mirror (get-bstLs bst))))))


(define (smallest-subTree x y bst)
 (cond ((and (< x (get-bstVal bst))
	     (< y (get-bstVal bst))) (smallest-subTree x y (get-bstLs bst)))
       ((and (> x (get-bstVal bst))
	     (> y (get-bstVal bst))) (smallest-subTree x y (get-bstRs bst)))
       (else bst)))

(define (getDepth bst bstVal)
 (cond ((not (isMemberbst? bstVal bst)) "Error")
       ((= bstVal (get-bstVal bst)) 0)
       ((> bstVal (get-bstVal bst)) (+ 1 (getDepth (get-bstRs bst) bstVal)))
       ((< bstVal (get-bstVal bst)) (+ 1 (getDepth (get-bstLs bst) bstVal)))))



;(define a '(10 (8 (6 () ()) (9 () ())) (12 (11 () ()) (13 () (14 () (15 () ()))))))
