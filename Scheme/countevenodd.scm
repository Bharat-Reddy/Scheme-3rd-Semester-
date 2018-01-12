(define (countEO ls)
 (list (count-even ls) (count-odd ls)))

(define (count-even ls)
 (if (null? ls)
     0
     (if (= (rem (car ls) 2) 0)
         (+ 1 (count-even (cdr ls)))
	 (+ 0 (count-even (cdr ls))))))

(define (count-odd ls)
 (if (null? ls)
     0
     (if (= (rem (car ls) 2) 1)
         (+ 1 (count-odd (cdr ls)))
	 (+ 0 (count-odd (cdr ls))))))

(define (rem a b)
 (if (< a b)
     a
     (if (= a b)
         0
	 (if (> a b)
	     (rem (- a b) b)))))
