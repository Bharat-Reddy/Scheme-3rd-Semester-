(define (oddindices ls)
 (if (null? ls)
     '()
     (cons (car ls) (oddindices (cddr ls)))))
     
