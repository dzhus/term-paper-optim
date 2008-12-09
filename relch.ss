#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/generic-ops
         pyani-lib/function-ops)

(provide relch-optimize
         gd-optimize
         gdn-optimize)

(define (gradient-method choose-shift stop-condition)
  (define (optimize function x-start iterations [listen-proc #f])
    (when listen-proc (listen-proc x-start))
    (if (<= iterations 0)
        x-start
        (let* ((g (@ (gradient function) x-start))
               (G (@ (hessian function) x-start))
               (shift (choose-shift x-start g G))
               (x-new (+ x-start shift)))
          (if (stop-condition function x-start x-new g G)
              x-new
              (optimize function x-new
                        (sub1 iterations)
                        listen-proc)))))
  optimize)

(define (zero-gradient-condition eps)
  (lambda (f x-start x-new g G)
    (<= (p-vector-norm g) eps)))

(define (close-arguments-condition eps)
  (lambda (f x-start x-new g G)
    (<= (p-vector-norm (- x-start x-new)) eps)))

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

;; Gradient descend
(define (make-gd-optimize regulate-shift stop-condition)
  (lambda (f x-start
        eps
        iterations [unused #f]
        [listen-proc #f])
    (define (choose-shift x-start g G)
      (let ((shift (* g -0.1)))
        (regulate-shift shift f x-start)))
    ((gradient-method choose-shift (stop-condition eps))
     f x-start iterations listen-proc)))

(define (make-relch-optimize regulate-shift stop-condition)
  (lambda (f x-start
        eps
        iterations
        degree
        [listen-proc #f])
    (define (choose-shift x-start g G)
      (define (relch-shift L g G)
        (let* ((n (matrix-size G))
               (d1 (zero-vector n))
               (d2 (* g -2)))
          (define (sub2 x) (sub1 (sub1 x)))
          ;; Calculate next degree of shift given shifts for two
          ;; previous degrees until degree reaches L
          (define (approach-target-degree [prev d2] [preprev d1] [degree 3])
            (let ((current-degree-shift
                   (+
                    (* (* (+ (identity-matrix n) (* G -2))
                          prev)
                       (/ (* 2 (sub1 degree)) degree))
                    (* preprev
                       (* -1 (/ (sub2 degree) degree)))
                    (* g (/ (* -4 (sub1 degree)) degree)))))
              (if (= degree L)
                  current-degree-shift
                  (approach-target-degree current-degree-shift prev (add1 degree)))))
          (cond ((= L 1) d1)
                ((= L 2) d2)
                (else (approach-target-degree d2 d1)))))
      (let* ((G (normalize-matrix G))
             (g (normalize-vector g))
             (shift (relch-shift degree g G)))
        (regulate-shift shift f x-start)))
    ((gradient-method choose-shift (stop-condition eps))
     f x-start iterations listen-proc)))

;; Different RELCH implementations
(define relch-optimize (make-relch-optimize regulate-shift zero-gradient-condition))
(define gd-optimize (make-gd-optimize regulate-shift zero-gradient-condition))
(define gdn-optimize (make-gd-optimize
                      (lambda (shift f x-start)
                        (regulate-shift (/ shift (p-vector-norm shift)) f x-start))
                      zero-gradient-condition))
