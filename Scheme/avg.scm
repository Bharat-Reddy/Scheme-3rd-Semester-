(define (average ls)
 (/ (sum ls) (len ls)))

(define (sum ls)
 (if (null? ls)
     0
     (+ (car ls) (sum (cdr ls)))))

(define (len ls)
 (if (null? ls)
     0
     (+ 1 (len (cdr ls)))))
