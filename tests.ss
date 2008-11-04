#lang scheme

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

;;; Test functions from «Applied Nonlinear Programming» by
;;; D. Himmelblau)

;; Rosenbrock function
(define (f1 x1 x2)
  (+ (* 100 (sqr (- x2 (sqr x1))))
     (sqr (- 1 x1))))

(define (f2 x1 x2)
  (+ (sqr (- x2 (sqr x1)))
     (sqr (- 1 x1))))

(define (f3 x1 x2)
  (+ (sqr (- x2 (sqr x1)))
     (* 100 (sqr (- 1 x1)))))

(define (f4 x1 x2)
  (+ (sqr (- 1.5 (* x1 (- 1 x2))))
     (sqr (- 2.25 (* x1 (- 1 (sqr x2)))))
     (sqr (- 2.625 (* x1 (- 1 (expt x2 3)))))))

(define (f5 x1 x2)
  (+ (sqr (- (+ (sqr x1) x2) 11))
     (sqr (- (+ x1 (sqr x2)) 7))))
