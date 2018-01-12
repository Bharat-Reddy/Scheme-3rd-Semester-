(define makeEmptybst '())
(define (make-bst bstVal bstLs bstRs) (list bstVal bstLs bstRs))
(define (get-bstVal bst) (car bst))
(define (get-bstLs bst) (cadr bst))
(define (get-bstRs bst) (caddr bst))
(define (isEmptybst? bst) (null? bst))


(define (isLeaf? bst) (and (isEmptybst? (get-bstLs bst))
		           (isEmptybst? (get-bstRs bst))))
(define (isMemberbst? bstVal bst) 
 (cond ((isEmptybst? bst) "Not Found")
       ((= bstVal (get-bstVal bst)) "found")
       ((< bstVal (get-bstVal bst)) (isMemberbst? bstVal (get-bstLs bst)))
       (else (isMemberbst? bstVal (get-bstRs bst)))))

(define (get-bstSize bst)
 (cond ((isEmptybst? bst) 0)
       (else (+ 1 (get-bstSize (get-bstLs bst))
	    (get-bstSize (get-bstRs bst))))))

(define (countLeaves bst)
 (cond ((isEmptybst? bst) 0)
       ((isLeaf? bst) 1)
       (else (+ (countLeaves (get-bstLs bst))
	        (countLeaves (get-bstRs bst))))))

(define (bstInsert bstVal bst)
 (cond ((isEmptybst? bst) (make-bst bstVal makeEmptybst makeEmptybst))
       ((eq? bstVal (get-bstVal bst)) bst)
       ((< bstVal (get-bstVal bst)) (make-bst (get-bstVal bst) (bstInsert bstVal (get-bstLs bst)) (get-bstRs bst)))
       (else (make-bst (get-bstVal bst) (get-bstLs bst) (bstInsert bstVal (get-bstRs bst))))))

(define (minheight bst)
 (if (isEmptybst? bst)
     0
     (min (+ 1 (minheight (get-bstLs bst)))
          (+ 1 (minheight (get-bstRs bst))))))

(define (bstDelete bstVal bst)
 (cond ((not (isMemberbst? bstVal bst)) bst)
       ((eq? bstVal (get-bstVal bst)) (deleteRoot bst))
       ((< bstVal (get-bstVal bst)) (make-bst (get-bstVal bst)
	                                      (bstDelete bstVal (get-bstLs bst))
					      (get-bstRs bst)))
       ((> bstVal (get-bstVal bst)) (make-bst (get-bstVal bst)
	                                      (get-bstLs bst)
	                                      (bstDelete bstVal (get-bstRs bst))))))

(define (deleteRoot bst)
 (cond ((isLeaf? bst) makeEmptybst)
       ((isEmptybst? (get-bstLs bst)) (get-bstRs bst))
       ((isEmptybst? (get-bstRs bst)) (get-bstLs bst))
       (else (make-bst (get-bstVal (get-LeftMostChild (get-bstRs bst)))
	               (get-bstLs bst)
		       (bstDelete (get-bstVal (get-LeftMostChild (get-bstRs bst))) (get-bstRs bst))))))


(define (get-LeftMostChild bst)
 (if (isEmptybst? (get-bstLs bst))
     bst
     (get-LeftMostChild (get-bstLs bst))))
     


(bstDelete (get-bstVal (get-LeftMostChild (get-bstRs '(10 (9 () ()) (13 () ()))))) '(10 (9 () ()) (13 () ())))


(define (sort bst)
 (if (isEmptybst? bst)
     '()
     (append (sort (get-bstLs bst)) (list (get-bstVal bst)) (sort (get-bstRs bst)))))

(define (minLeaf bst)
 (cond ((isLeaf? bst) 0)
       ((isEmptybst? (get-bstLs bst)) (+ 1 (minLeaf (get-bstRs bst))))
       ((isEmptybst? (get-bstRs bst)) (+ 1 (minLeaf (get-bstLs bst))))
       (else (min (+ 1 (minLeaf (get-bstLs bst)))
	          (+ 1 (minLeaf (get-bstRs bst)))))))




