#lang scheme

(require srfi/43
         (planet wmfarr/simple-matrix:1:0/matrix)
         pyani-lib/vector
         pyani-lib/matrix)

(provide normalize-vector
         normalize-matrix
         max-slice)

(define (normalize-vector v)
  (vector-scale v (/ (p-vector-norm v))))

(define (normalize-matrix m)
  (matrix-scale m (/ (euclidean-matrix-norm m))))

(define (max-slice g)
  (lambda (h . x) (max 0 (apply g (append (list h) x)))))
