(define (sort ls)
 (if (null? ls)
     '()
     (cons (minimum ls) (sort (remove-elem ls (minimum ls))))))

(define (minimum ls)
 (cond ((null? (cdr ls)) (car ls))
       ((< (car ls) (minimum (cdr ls))) (car ls))
       (else (minimum (cdr ls)))))

(define (remove-elem ls n)
 (if (= (car ls) n)
     (cdr ls)
     (cons (car ls) (remove-elem (cdr ls) n))))
