#lang scheme

;;; Operations for vectors

(require srfi/43
         "get-put.ss")

(provide vector-+-vector
         install-vector-generics-package)

(define (vector-+-vector v1 v2)
  (vector-map (lambda (i x y) (+ x y)) v1 v2))

(define (install-vector-generics-package)
  (put 'add '(vector vector) vector-+-vector))