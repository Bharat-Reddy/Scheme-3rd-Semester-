(define (getFirstElement Queue)
  (car Queue))
(define (getRestElements Queue)
  (cdr Queue))
(define (getArrTime elem)
  (cadr elem))


(define (getFrontElement Queue)
  (if(isEmptyQ? Queue)
     'error
     (getFirstElement Queue)))

(define (enqueue Queue element)
  (if(isEmptyQ? Queue)
     (list element)
     (cons (getFrontElement Queue) (enqueue (getRestElements Queue) element))))



(define (isEmptyQ? Queue)
  (null? Queue))


(define (newQ q1 q2)
  (if (isEmptyQ? q2)
     q1
     (getRevOrderArrTime (newQ (enqueue q1 (getFrontElement q2)) (getRestElements q2)))))
(define (getRevOrderArrTime Queue)
  (if(isEmptyQ? Queue)
     Queue
     (cons (maxArrTime Queue) (getRevOrderArrTime (deletemax Queue)))))
(define (deletemax Queue)
  (if(equal? (getFirstElement Queue) (maxArrTime Queue))
     (getRestElements Queue)
     (cons (getFirstElement Queue) (deletemax (getRestElements Queue)))))
(define (maxArrTime Queue)
  (if(isEmptyQ? (getRestElements Queue))
     (getFirstElement Queue)
     (maximum (getFirstElement Queue) (maxArrTime (getRestElements Queue)))))
(define (maximum a b)
  (if(> (getArrTime a) (getArrTime b))
        a
        b))
(define (getArrTime elem)
   (cadr elem))


