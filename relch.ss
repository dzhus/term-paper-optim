#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/generic-ops)

(provide relch-optimize)

(define @ at-vector)

(define (relch-shift L G g)
  (define (sub2 x) (sub1 (sub1 x)))
  (let ((n (matrix-size G)))
    (cond ((= L 1) (zero-vector n))
          ((= L 2) (* g -2))
          (else (+
                 (* (* (+ (identity-matrix n) (* G -2))
                       (relch-shift (sub1 L) G g))
                    (/ (* 2 (sub1 L)) L))
                 (*
                  (relch-shift (sub2 L) G g)
                  (* -1 (/ (sub2 L) L)))
                 (* g (/ (* -4 (sub1 L)) L)))))))

(define (gradient-method choose-shift make-shifted)
  (define (optimize function
                    x-start
                    #:eps [eps 1e-2]
                    #:iterations iterations
                    #:listener [listen-proc #f])
    (let ((g (@ (gradient function) x-start)))
      (when listen-proc (listen-proc x-start))
      (if (or (<= iterations 1) (< (p-vector-norm g) eps))
          x-start
          (let* ((G (normalize-matrix (@ (hessian function) x-start)))
                 (g (normalize-vector g))
                 (shift (choose-shift x-start G g))
                 (x-new (make-shifted x-start shift)))
            (optimize function x-new
                      #:eps eps
                      #:iterations (sub1 iterations)
                      #:listener listen-proc)))))
  optimize)

(define (relch-optimize f x-start
                        #:eps eps
                        #:iterations iterations
                        #:degree L
                        #:listener [listen-proc #f])
  (define (choose-shift x-start G g)
    (relch-shift L G g))
  (define (make-shifted x shift [alpha 1])
    (define (better-solution? x-new x)
      (< (@ f x-new) (@ f x)))
    (let ((x-new (+ x (* shift alpha))))
      (if (better-solution? x-new x)
          x-new
          (make-shifted x shift (/ alpha 2)))))
  ((gradient-method choose-shift make-shifted)
   f x-start #:eps eps #:iterations iterations #:listener listen-proc))
