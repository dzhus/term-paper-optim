#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/generic-ops)

(provide optimize)

(define (shift L G g)
  (define (sub2 x) (sub1 (sub1 x)))
  (let ((n (matrix-size G)))
    (cond ((= L 1) (zero-vector n))
          ((= L 2) (* g -2))
          (else (+
                 (* (* (+ (identity-matrix n) (* G -2))
                       (shift (sub1 L) G g))
                    (/ (* 2 (sub1 L)) L))
                 (*
                  (shift (sub2 L) G g)
                  (* -1 (/ (sub2 L) L)))
                 (* g (/ (* -4 (sub1 L)) L)))))))

(define (optimize f x
                  #:eps [eps 1e-3]
                  #:iterations [iterations 20]
                  #:L [L 10]
                  #:listener [listen-proc #f])
  (define @ at-vector)
  (define (better-solution? x-new)
    (< (@ f x-new) (@ f x)))
  (define (make-good-shifted shift [alpha 1])
    (let ((x-new (+ x (* shift alpha))))
      (if (better-solution? x-new)
          x-new
          (make-good-shifted shift (/ alpha 2)))))
  (let ((g (@ (gradient f) x)))
    (listen-proc (format "~a" x))
    (if (or (<= iterations 1) (< (p-vector-norm g) eps))
        x
        (let* ((g (normalize-vector g))
               (G (normalize-matrix (@ (hessian f) x)))
               (shift (shift L G g)))
          (let ((x-new (make-good-shifted shift)))
            (optimize f x-new
                      #:eps eps
                      #:iterations (sub1 iterations)
                      #:L L
                      #:listener listen-proc))))))
