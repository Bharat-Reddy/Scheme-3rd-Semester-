(define make-emptybst '())
(define (make-bst bstVal bstLs bstRs) (list bstVal bstLs bstRs))
(define (get-bstVal bst) (car bst))
(define (get-bstLs bst) (cadr bst))
(define (get-bstRs bst) (caddr bst))
(define (isEmptybst? bst) (null? bst))

(define (is-leafbst? bst)
 (and (isEmptybst? (get-bstLs bst))
      (isEmptybst? (get-bstRs bst))))

(define (isMember? bstVal bst)
 (cond ((isEmptybst? bst) #f)
       ((= bstVal (get-bstVal bst)) #t)
       ((< bstVal (get-bstVal bst)) (isMember? bstVal (get-bstLs bst)))
       ((> bstVal (get-bstVal bst)) (isMember? bstVal (get-bstRs bst)))))

(define (get-size bst)
 (cond ((isEmptybst? bst) 0)
       (else (+ 1
	        (get-size (get-bstLs bst))
		(get-size (get-bstRs bst))))))

(define (get-height bst)
 (cond ((isEmptybst? bst) 0)
       (else (+ 1 (max (get-height (get-bstLs bst))
	         (get-height (get-bstRs bst)))))))

(define (no-of-leaves bst)
 (cond ((isEmptybst? bst) 'Empty-bst) 
       ((is-leafbst? bst) 1)
       (else (+ (no-of-leaves (get-bstLs bst))
	        (no-of-leaves (get-bstRs bst))))))

(define (get-min-dist-to-leaf bst)
 (cond ((is-Leafbst? bst) 0)
       ((isEmptybst? (get-bstLs bst)) (+ 1 (get-min-dist-to-leaf (get-bstRs bst))))
       ((isEmptybst? (get-bstRs bst)) (+ 1 (get-min-dist-to-leaf (get-bstLs bst))))
       (else (+ 1 (min (get-min-dist-to-leaf (get-bstLs bst))
		       (get-min-dist-to-leaf (get-bstRs bst)))))))

(define (get-max bst)
 (cond ((isEmptybst? (get-bstRs bst)) (get-bstVal bst))
       (else (get-max (get-bstRs bst)))))


(define (bst-insert bstVal bst)
 (cond ((isEmptybst? bst) (make-bst bstVal make-emptybst make-emptybst))
       ((= bstVal (get-bstVal bst)) bst)
       ((< bstVal (get-bstVal bst)) (make-bst (get-bstVal bst)
	                                      (bst-insert bstVal (get-bstLs bst))
					      (get-bstRs bst)))
       (else (make-bst (get-bstVal bst)
	               (get-bstLs bst)
		       (bst-insert bstVal (get-bstRs bst))))))


(define (bst-delete bstVal bst)
 (cond ((not (isMember? bstVal bst)) bst)
       ((< bstVal (get-bstVal bst)) (make-bst (get-bstVal bst)
	                                      (bst-delete bstVal (get-bstLs bst))
					      (get-bstRs bst)))
       ((> bstVal (get-bstVal bst)) (make-bst (get-bstVal bst)
	                                      (get-bstLs bst)
					      (bst-delete bstVal (get-bstRs bst))))
	(else (delete-root bst))))

(define (delete-root bst)
 (cond ((is-leafbst? bst) make-emptybst)
       ((isEmptybst? (get-bstLs bst)) (get-bstRs bst))
       ((isEmptybst? (get-bstRs bst)) (get-bstLs bst))
       (else (make-bst (get-bstVal (get-leftMostChild (get-bstRs bst)))
	               (get-bstLs bst)
		       (bst-delete (get-bstVal (get-leftMostChild (get-bstRs bst)))
			           (get-bstRs bst))))))

(define (get-leftMostChild bst)
 (cond ((isEmptybst? (get-bstLs bst)) bst)
       (else (get-leftMostChild (get-bstLs bst)))))

(define (in-order bst)
 (cond ((isEmptybst? bst) make-emptybst)
        (else (append (in-order (get-bstLs bst)) (list (get-bstVal bst)) (in-order (get-bstRs bst))))))

(define (pre-order bst)
 (cond ((isEmptybst? bst) make-emptybst)
       (else (append (pre-order (get-bstLs bst)) (pre-order (get-bstRs bst)) (list (get-bstVal bst))))))


(define (descending bst)
 (if (isEmptybst? )
     make-emptybst
     (append (descending (get-bstRs bst)) (list (get-bstVal bst)) (descending (get-bstLs bst)))))

(define (max-depth bst)
 (cond ((isEmptybst? bst) 0)
       (else (+ 1 (max (max-depth (get-bstLs bst))
	         (max-depth (get-bstRs bst)))))))

(define (get-minValue bst)
 (if (isEmptybst? (get-bstLs bst))
     (get-bstVal bst)
     (get-minValue (get-bstLs bst))))

(define (max-hasPathSum bst)
 (cond ((is-leafbst? bst) (get-bstVal bst))
       ((isEmptybst? (get-bstLs bst)) (+ (get-bstVal bst) (max-hasPathSum (get-bstRs bst))))
       ((isEmptybst? (get-bstRs bst)) (+ (get-bstVal bst) (max-hasPathSum (get-bstLs bst))))
       (else (+ (get-bstVal bst) (max (max-hasPathSum (get-bstRs bst))
				      (max-hasPathSum (get-bstLs bst)))))))

'(10 (5 (4 () ()) (8 (6 () (7 () ())) (9 () ()))) (15 () ()))

;'(5 (4 (3 () ()) ()) (10 (7 (6 () ()) (8 () ())) ()))

       
       
