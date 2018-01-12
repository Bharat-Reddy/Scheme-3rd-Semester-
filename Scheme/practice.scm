(define (main ecom-db dept-name manufacturer)
 (minimum (get-required-prod-recs (get-required-dept-prod-recs ecom-db dept-name) manufacturer)))

(define (get-required-dept-prod-recs ecom-db dept-name)
 (if (empty? ecom-db) 
     '()
     (if (eq? (get-dept-name (get-first-dept-record ecom-db)) dept-name)
              (get-prod-recs (get-first-dept-record ecom-db))
	      (get-required-dept-prod-recs (get-rest-dept-records ecom-db) dept-name))))

(define (empty? ls) (null? ls))

(define (get-required-prod-recs d-prod-recs manufacturer)
 (if (empty? d-prod-recs)
     '()
     (if (eq? (get-manufacturer (get-first-prod-record d-prod-recs)) manufacturer)
         (cons (get-first-prod-record d-prod-recs) 
	       (get-required-prod-recs (get-rest-prod-records d-prod-recs) manufacturer))
	 (get-required-prod-recs (get-rest-prod-records d-prod-recs) manufacturer))))

(define (minimum prod-recs)
 (cond ((empty? prod-recs) "No Products Found")
       ((empty? (get-rest-prod-records prod-recs)) (get-first-prod-record prod-recs))
       ((< (get-price (get-first-prod-record prod-recs)) (get-price (minimum (get-rest-prod-records prod-recs)))) (get-first-prod-record prod-recs))
       (else (minimum (get-rest-prod-records prod-recs)))))

; Selectors::>>

(define (get-first-dept-record ecom-db) (car ecom-db))
(define (get-rest-dept-records ecom-db) (cdr ecom-db))

(define (get-dept-name dept-record) (car dept-record))
(define (get-prod-recs dept-record) (cadr dept-record))

(define (get-first-prod-record  prod-recs) (car prod-recs))
(define (get-rest-prod-records prod-recs) (cdr prod-recs))

(define (get-manufacturer prod-record) (cadr prod-record))
(define (get-price prod-record) (caddr prod-record))



'((a ((1 p 1) (1 p 2) (1 g 3) (1 p 4))) (b ((1 p 1) (1 p 2) (1 g 3) (1 p 0))) (c ((1 p 1) (1 p 3) (1 g 8) (1 p 0))) (d ((1 p 1) (1 p 2) (1 g 0) (1 p 4))) (e ((1 p 0) (1 p 1) (1 g 8) (1 p 2))))













