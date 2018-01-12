(define (remove-duplicates ls)
 (if (null? ls)
     '()
     (if (search (car ls) (cdr ls))
         (remove-duplicates (cdr ls))
	 (cons (car ls) (remove-duplicates (cdr ls))))))

(define (search n ls)
 (if (null? ls)
     #f
     (if (= n (car ls))
         #t
         (search n (cdr ls)))))
