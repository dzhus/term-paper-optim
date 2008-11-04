#lang scheme

(require srfi/43
         "matrix.ss")

(provide deriv gradient hessian)

(define dx 1e-6)

;; Derivative (partial probably)
;; 
;; (numbers -> numbers) -> (numbers -> numbers)
(define (deriv f [arg 1] [dx dx])
  (define (der . args)
    (/
     (-
      (apply f
             (append
              (take args (sub1 arg))
              (list (+ (list-ref args (sub1 arg)) dx))
              (drop args arg)))
      (apply f args))
     dx))
  der)

;; Gradient of a function (still a vector for unary function)
;;
;; (numbers -> numbers) -> (numbers -> vector)
(define (gradient f [dx dx])
  (let ((n (procedure-arity f)))
    (lambda args
      (build-vector n
                    (lambda (i)
                      (apply (deriv f (add1 i) dx) args))))))

;; Hessian matrix
;; 
;; (numbers -> numbers) -> (numbers -> matrix)
(define (hessian f [dx dx])
  (let ((n (procedure-arity f)))
    (lambda args
      (build-matrix (lambda (i j)
                      (apply (deriv
                              (deriv f (add1 i) dx) (add1 j) dx)
                             args))
                    n n))))

;; `(at-vector f '#(a b))` is `(f a b)`
(define (at-vector f v)
  (apply f (vector->list v)))
