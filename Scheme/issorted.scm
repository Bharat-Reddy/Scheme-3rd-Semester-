(define (is-sorted ls)
 (or (is-assending? ls) (is-descending? ls)))

(define (is-assending? ls)
 (if (null? (cdr ls))
     #t
     (if (< (car ls) (cadr ls))
         (is-assending? (cdr ls))
         #f)))

(define (is-descending? ls)
 (if (null? (cdr ls))
     #t
     (if (> (car ls) (cadr ls))
         (is-descending? (cdr ls))
         #f)))
