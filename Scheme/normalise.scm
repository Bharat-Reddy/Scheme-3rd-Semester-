(define (normalise ls)
 (helper ls (sum ls)))

(define (sum ls)
 (if (null? ls)
     0
     (+ (car ls) (sum (cdr ls)))))

(define (helper ls n)
 (if (null? ls)
     '()
     (cons (/ (car ls) n) (helper (cdr ls) n))))
