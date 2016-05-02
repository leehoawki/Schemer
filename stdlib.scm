(define (not x)            (if x #f #t))
(define (list . objs)      objs)
(define (id obj)           obj)
(define (flip func)        (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1)  (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g)      (lambda (arg) (f (apply g arg))))
(define (null? obj)        (if (eqv? obj '()) #t #f))
(define (foldr func end lst) (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))
(define (sum . lst)         (foldr + 0 lst))
(define (product . lst)     (foldr * 1 lst))