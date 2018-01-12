(define (elem-sum ls)
 (sum ls (rev ls '())))

(define (sum ls1 ls2)
 (if (null? ls1)
     '()
     (cons (+ (car ls1) (car ls2)) (sum (cdr ls1) (cdr ls2)))))

(define (rev ls nls)
 (if (null? ls)
     nls
     (rev (cdr ls) (cons (car ls) nls))))
