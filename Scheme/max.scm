(define (maxi ls)
 (cond ((null? (cdr ls)) (car ls))
       ((> (car ls) (maxi (cdr ls))) (car ls))
       (else (maxi (cdr ls)))))
