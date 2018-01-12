(define carried-plus
 (lambda (x)
  (lambda (y) (+ x y))))

(define composite
 (lambda (f g)
  (lambda (x) (f (g x)))))
