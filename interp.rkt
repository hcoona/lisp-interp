#lang racket
(require racket/match)

(define env0 '())

(define (ext-env k v env)
  (cons `(,k . ,v) env))

(define (ext-env-lst ks vs env)
  (if (empty? ks)
      env
      (ext-env-lst (cdr ks)
                   (cdr vs)
                   (ext-env (car ks)
                            (car vs)
                            env))))

(define (lookup-env k env)
  (cdr (assoc k env)))

(struct Closure (exp env) #:transparent)

(define (eval1. exp env)
  (let ([eval. (lambda (exp) (eval1. exp env))])
    (match exp
      ['#t #t]
      ['#f #f]
      [(? number? x) x]
      [(? symbol? x) (lookup-env x env)]
      [`(quote ,x) x]
      [`(atom? ,x) (symbol? (eval. x))]
      [`(eq? ,x ,y) (eq? (eval. x) (eval. y))]
      [`(car ,x) (car (eval. x))]
      [`(cdr ,x) (cdr (eval. x))]
      [`(cons ,x ,y) (cons (eval. x) (eval. y))]
      [`(cond . ,pairs)
       (let eval-cond ([p (car pairs)]
                       [r (cdr pairs)])
         (if (eval. (car p))
             (eval. (cadr p))
             (eval-cond (car r) (cdr r))))]
      [`(lambda (,x ...) ,e)
       (Closure exp env)]
      [`(label ,id (lambda (,x ...) ,e))
       (Closure `(lambda ,x ,e)
                (ext-env id
                         exp
                         env))]
      [`(defun ,id (,x ...) ,e)
       (eval. `(label ,id (lambda ,x ,e)))]
      [`(+ ,e-lst ...)
       (let ([v-params (map eval. e-lst)])
         (apply + v-params))]
      [`(,op ,e-lst ...)
       (let eval-op. ([v-op (eval. op)]
                      [v-params (map eval. e-lst)])
         (match v-op
           [(Closure `(lambda (,params ...) ,e) env1)
            (eval1. e (ext-env-lst params v-params env1))]
           [`(label ,id ,f)
            (eval-op. (eval. f) v-params)]))])))

(define (eval. exp)
  (eval1. exp env0))

(provide eval.)