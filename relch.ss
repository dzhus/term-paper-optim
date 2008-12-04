#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/generic-ops)

(provide relch-optimize
         gd-optimize)

(define @ at-vector)

(define (gradient-method choose-shift stop-condition)
  (define (optimize function x-start iterations [listen-proc #f])
    (when listen-proc (listen-proc x-start))
    (if (or (<= iterations 1)
            (stop-condition function x-start))
        x-start
        (let* ((G (@ (hessian function) x-start))
               (g (@ (gradient function) x-start))
               (shift (choose-shift x-start G g))
               (x-new (+ x-start shift)))
          (optimize function x-new
                    (sub1 iterations)
                    listen-proc))))
  optimize)

(define (zero-gradient? f x eps)
  (<= (p-vector-norm (@ (gradient f) x)) eps))

(define (decrease-shift shift)
  (/ shift 2))

(define (better-minimum? f x-new x)
  (< (@ f x-new) (@ f x)))

;; Decrease shift factor until it leads to a better minimum
(define (regulate-shift shift f x-start)
  (let ((x-new (+ x-start shift)))
    (if (better-minimum? f x-new x-start)
        shift
        (regulate-shift (decrease-shift shift) f x-start))))

;; Simple RELCH implementation as proposed by Chernorutsky
(define (relch-optimize f x-start
                        eps
                        iterations
                        degree
                        [listen-proc #f])
  (define (choose-shift x-start G g)
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
    (let* ((G (normalize-matrix G))
           (g (normalize-vector g))
           (shift (relch-shift degree G g)))
      (regulate-shift shift f x-start)))
  ((gradient-method choose-shift (lambda (f x) (zero-gradient? f x eps)))
   f x-start iterations listen-proc))

;; Gradient descend
(define (gd-optimize f x-start
                     eps
                     iterations [unused #f]
                     [listen-proc #f])
  (define (choose-shift x-start G g)
    (regulate-shift (/ (* g -1) (p-vector-norm g)) f x-start))
  ((gradient-method choose-shift (lambda (f x) (zero-gradient? f x eps)))
   f x-start iterations listen-proc))
