#lang racket
(require rackunit "interp.rkt")

(check-equal? (eval. '(quote x)) 'x "Primitive quote")

(check-equal? (eval. '(atom? 'a)) #t "Primitive atom?")
(check-equal? (eval. '(atom? '(a b c))) #f "Primitive atom?")

(eval. '(eq? 'a 'a))
(eval. '(eq? 'a 'b))
(eval. '(eq? '() '()))

(eval. '(car '(a b c)))
(eval. '(cdr '(a b c)))
(eval. '(cons 'a '(b c)))

(eval. '(cond [(eq? 'a 'b) 'first]
              [(atom? 'a) 'second]))

(eval. '((lambda (x) (cons x '(b))) 'a))
(eval. '((lambda (x y z) (cons (cons x y) z)) 'a '(b) '(c)))

(eval. '((label firstatom (lambda (x)
                            (cond ((atom? x) x)
                                  (#t (firstatom (car x))))))
         '((a b) (c d))))
(eval. '((label length (lambda (x)
                         (cond [(eq? x '()) 0]
                               [#t (+ 1 (length (cdr x)))])))
         '(1 2 3 4 5)))

(eval. '((defun length (x)
           (cond [(eq? x '()) 0]
                 [#t (+ 1 (length (cdr x)))]))
         '(1 2 3)))