#lang scheme

;;; Test functions from «Applied Nonlinear Programming» by
;;; D. Himmelblau

(require srfi/1)

(provide (rename-out [d:make-test-function make-test-function])
         test-function-name
         test-function-def
         test-function-x-start
         test-function-target-x
         test-function-target-value
         test-function-comment
         test-functions)

;; TODO Support several extremal points
(define-struct test-function (name def
                                   x-start
                                   target-x target-value
                                   comment))

(define (d:make-test-function name def x-start
                              target-x target-value
                              [comment #f])
  (make-test-function name def x-start
                      target-x target-value
                      comment))

(define test-functions
  (list
   ;; Rosenbrock function
   (cons
    "rosenbrock"
    (d:make-test-function
     "100(x₂ - x₁²)² + (1 - x₁)²"
     (lambda (x1 x2)
       (+ (* 100 (sqr (- x2 (sqr x1))))
          (sqr (- 1 x1))))
     '#(-1.2 1)
     '#(1 1) 0 "Тест Розенброка"))

   (cons
    "rosenbrock-mini"
    (d:make-test-function
     "(x₂ - x₁²)² + (1 - x₁)²"
     (lambda (x1 x2)
       (+ (sqr (- x2 (sqr x1)))
          (sqr (- 1 x1))))
     '#(-1.2 1)
     '#(1 1) 0))

   (cons
    "rosenbrock-maxi"
    (d:make-test-function
     "(x₂ - x₁³)² + 100(1 - x₁)²"
     (lambda (x1 x2)
       (+ (sqr (- x2 (expt x1 3)))
          (* 100 (sqr (- 1 x1)))))
     '#(-1.2 1)
     '#(1 1) 0))
   
   (cons
    "exptest"
    (d:make-test-function
     "∑[exp(-xa)-exp(-ya)-(exp(-a)-exp(-10a))]²"
     (lambda (x y)
       (apply +
              (map (lambda (a)
                     (sqr (- (- (exp (* (- x) a)) (exp (* (- y) a)))
                             (- (exp (- a)) (exp (* -10 a))))))
                   (iota 10 0.1 0.1))))
     '#(2 -1)
     '#(1 10) 0))

   (cons
    "hess-singular"
    (d:make-test-function
     "(x₁ + 10x₂)² + 5(x₃ - x₄)² + (x₂-2x₃)⁴+10(x₁-x₄)⁴"
     (lambda (x1 x2 x3 x4)
       (+ (sqr (+ x1 (* 10 x2)))
          (* 5 (sqr (- x3 x4)))
          (expt (- x2 (* 2 x3)) 4)
          (* 10 (expt (- x1 x4) 4))))
     '#(-3 -1 0 1)
     '#(0 0 0 0) 0 "Сингулярная матрица Гессе в точке минимума"))

   (cons
    "himmelblau"
    (d:make-test-function
     "(x₁² + x₂ - 11)² + (x₁ + x₂² - 7)²"
     (lambda (x1 x2)
       (+ (sqr (- (+ (sqr x1) x2) 11))
          (sqr (- (+ x1 (sqr x2)) 7))))
     '#(1 1)
     '(#(3 2) #(3.58 -1.84) #(-3.78 -3.28) #(-2.81 3.13)) 0))))
