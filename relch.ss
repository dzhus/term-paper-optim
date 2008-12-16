#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         "gradient-methods.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/generic-ops
         pyani-lib/function-ops)

(provide relch-optimize
         gd-optimize optimization-method?
         sgd-optimize optimization-method?
         gdn-optimize optimization-method?)


;; Shift regulations enforce relaxation condition enforcement for both
;; RELCH and GD, they also provide basic fallback measure in case of
;; infinite shifts

;; Decrease shift factor until it leads to a better value
(define (enforce-relaxation shift f x-start)
  (define (decrease-shift shift)
    (/ shift 2))
  (define (better-minimum? f x-new x)
    (< (@ f x-new) (@ f x)))
  (let ((x-new (+ x-start shift)))
    (if (better-minimum? f x-new x-start)
        shift
        (enforce-relaxation (decrease-shift shift) f x-start))))

;; Enforce relaxation only if it's possible; in case of numeric
;; overflow leading to infinite shift, return semirandom shift
(define (enforce-relaxation-filtered shift f x-start)
  (define (random-vector length)
    (build-vector length (lambda (x) (* 2 (- (random 2) 1/2)))))
  (if (< (p-vector-norm shift) +inf.0)
      (enforce-relaxation shift f x-start)
      (random-vector (vector-length shift))))

(define (make-sgd-regulate factor)
  (lambda (shift f x-start)
    (* shift factor)))


;; Gradient descend
(define (make-gd-optimize regulate-shift stop-condition)
  (lambda (f x-start
        eps
        iterations [unused #f]
        [listener void])
    (define (choose-shift x-start g G)
      (let ((shift (* g -1)))
        (regulate-shift shift f x-start)))
    ((gradient-method choose-shift (stop-condition eps))
     f x-start iterations listener)))

(define (make-relch-optimize regulate-shift stop-condition)
  (lambda (f x-start
        eps
        iterations
        degree
        [listener void])
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
     f x-start iterations listener)))

;; RELCH with infinite shift filtering and step regulation
(define relch-optimize (make-relch-optimize enforce-relaxation-filtered
                                            zero-gradient-condition))

;; Crude fixed step GD (used to measure gully)
(define (sgd-optimize f x-start eps iterations step-factor [listener void])
  (let ((optimize (make-gd-optimize (make-sgd-regulate step-factor)
                                    zero-gradient-condition)))
    (optimize f x-start eps iterations step-factor listener)))

;; GD with regulation (quickly gets to gully)
(define gd-optimize (make-gd-optimize enforce-relaxation
                                      zero-gradient-condition))

(define gdn-optimize (make-gd-optimize (lambda (shift f x-start)
                                         (enforce-relaxation (/ shift (p-vector-norm shift)) f x-start))
                                       zero-gradient-condition))
