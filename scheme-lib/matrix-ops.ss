#lang scheme

;;; Operations for matrices

(require "matrix.ss"
         "get-put.ss")

(provide matrix-+-matrix
         install-matrix-generics-package)

(define (matrix-+-matrix m1 m2)
  (let ((m (matrix-rows m1))
        (n (matrix-columns m1)))
    (build-matrix (lambda (i j)
                    (+ (matrix-item m1 i j)
                       (matrix-item m2 i j)))
                  m n)))

(define (install-matrix-generics-package)
  (put 'add '(matrix matrix) matrix-+-matrix))