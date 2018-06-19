(define list (lambda x x))
(define null? (lambda (x) (eqv? x '())))
(define length (lambda (x) (if (null? x) 0 (+ 1 (length (cdr x))))))

(define zero? (lambda (n) (= n 0)))
(define positive? (lambda (n) (> n 0)))
(define negative? (lambda (n) (< n 0)))

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
