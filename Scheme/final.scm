(define (getLeft bt) (cadr bt))
(define (getRight bt) (caddr bt))
(define (getRootValue bt) (car bt))
(define (isEmpty? bt) (null? bt))

(define (isLeaf? bt)
 (and (isEmpty? (getLeft bt))
      (isEmpty? (getRight bt))))

(define (isMember? bt c)
(cond ((isEmpty? bt) #f)
      ((equal? (getRootValue bt) c) #t)
      (else (or (isMember? (getLeft bt) c)
	        (isMember? (getRight bt) c)))))

(define (areMembers? bt c1 c2)
 (and (isMember? bt c1)
      (isMember? bt c2)))

(define (getDepth bt c)
 (cond ((equal? c (getRootValue bt)) 0)
       ((isMember? (getLeft bt) c) (+ 1 (getDepth (getLeft bt) c)))
       (else (+ 1 (getDepth (getRight bt) c)))))

(define (getSubtree bt c1 c2)
 (cond ((areMembers? (getLeft bt) c1 c2) (getSubtree (getLeft bt) c1 c2))
       ((areMembers? (getRight bt) c1 c2) (getSubtree (getRight bt) c1 c2))
       (else bt)))

(define (getNumberofMidCities bt c1 c2)
 (if (= c1 c2)
     0
     (- (+ (getDepth (getSubtree bt c1 c2) c1)
           (getDepth (getSubtree bt c1 c2) c2)) 1)))

(define (getFarthestDistance bt c)
 (helper bt bt c))

(define (helper original-bt bt c)
 (cond ((isLeaf? bt) (getNumberofMidCities original-bt c (getRootValue bt)))
       ((isEmpty? (getLeft bt)) (max (getNumberofMidCities original-bt c (getRootValue bt))
	                             (helper original-bt (getRight bt) c)))
       ((isEmpty? (getRight bt)) (max (getNumberofMidCities original-bt c (getRootValue bt))
	                              (helper original-bt (getLeft bt) c)))
       (else (max (getNumberofMidCities original-bt c (getRootValue bt))
	          (helper original-bt (getRight bt) c)
		  (helper original-bt (getLeft bt) c)))))










