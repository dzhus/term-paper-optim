#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         "scheme-lib/matrix.ss"
         "scheme-lib/vector.ss"
         "scheme-lib/generic-ops.ss")

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

(define (optimize f x [iterations 10] [L 10])
  (define @ at-vector)
  (define (better-solution? x-new)
    (< (@ f x-new) (@ f x)))
  (define (make-good-shifted shift [alpha 1])
    (let ((x-new (+ x (* shift alpha))))
      (if (better-solution? x-new)
          x-new
          (make-good-shifted shift (/ alpha 2)))))
  (if (<= iterations 1)
      x
      (let* ((g (normalize-vector (@ (gradient f) x)))
             (G (normalize-matrix (@ (hessian f) x)))
             (shift (shift L G g)))
        (let ((x-new (make-good-shifted shift)))
          (optimize f x-new (sub1 iterations))))))