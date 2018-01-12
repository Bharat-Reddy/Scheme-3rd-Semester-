(define (get-mid ls)
 (list-reference ls (/ (+ 1 (length ls)) 2)))

(define (list-reference ls n)
 (if (= n 1)
     (car ls)
     (list-reference (cdr ls) (- n 1))))
