#lang scheme

;;; Test functions from «Applied Nonlinear Programming» by
;;; D. Himmelblau

(require srfi/1
         "shared.ss")

(provide (rename-out [d:make-test-problem make-test-problem])
         test-problem-name
         test-problem-function
         test-problem-x-start
         test-problem-target-x
         test-problem-target-value
         test-problem-comment
         test-problems)

(define-struct test-problem (name function
                                  x-start
                                  target-x target-value
                                  comment))

(define (d:make-test-problem name function x-start
                              target-x target-value
                              [comment #f])
  (make-test-problem name function x-start
                      target-x target-value
                      comment))

(define test-problems
  (list
   ;; Rosenbrock function
   (cons
    "rosenbrock"
    (d:make-test-problem
     "100(x₂ - x₁²)² + (1 - x₁)²"
     (lambda (x1 x2)
       (+ (* 100 (sqr (- x2 (sqr x1))))
          (sqr (- 1 x1))))
     '#(-1.2 1)
     '#(1 1) 0 "Тест Розенброка"))

   (cons
    "rosenbrock-mini"
    (d:make-test-problem
     "(x₂ - x₁²)² + (1 - x₁)²"
     (lambda (x1 x2)
       (+ (sqr (- x2 (sqr x1)))
          (sqr (- 1 x1))))
     '#(-1.2 1)
     '#(1 1) 0))

   (cons
    "rosenbrock-maxi"
    (d:make-test-problem
     "(x₂ - x₁³)² + 100(1 - x₁)²"
     (lambda (x1 x2)
       (+ (sqr (- x2 (expt x1 3)))
          (* 100 (sqr (- 1 x1)))))
     '#(-1.2 1)
     '#(1 1) 0))
   
   (cons
    "exptest"
    (d:make-test-problem
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
    (d:make-test-problem
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
    (d:make-test-problem
     "(x₁² + x₂ - 11)² + (x₁ + x₂² - 7)²"
     (lambda (x1 x2)
       (+ (sqr (- (+ (sqr x1) x2) 11))
          (sqr (- (+ x1 (sqr x2)) 7))))
     '#(1 1)
     '(#(3 2) #(3.584428 -1.848126) #(-3.77931 -3.283186) #(-2.805118 3.131312)) 0))
   
   (cons
    "penalty"
    (d:make-test-problem
     "(x₁+4)² + (x₂ - 4)² → min"
     (let ((f (lambda (x1 x2) (+ (sqr (+ x1 4)) (sqr (- x2 4)))))
           (g1 (lambda (x1 x2) (- (- (* 2 x1) x2) 2)))
           (g2 (lambda (x1 x2) (- x1)))
           (g3 (lambda (x1 x2) (- x2))))
       (lambda (x y)
         (+ (f x y) (* 10000 (+ (sqr ((max-slice g1) x y))
                                (sqr ((max-slice g2) x y))
                                (sqr ((max-slice g3) x y)))))))
     '#(-1 -1)
     #(0 4) 16))

   (cons
    "gully"
    (d:make-test-problem
     "(x²+12y-1)²+(49x²+49y+84x-681)²"
     (lambda (x y)
       (+ (sqr (+ (sqr x) (* 12 y) -1))
          (sqr (+ (* 49 (sqr x)) (* 49 y) (* 84 x) -681))))
     '#(-1 2)
     '#(0.2 0.9) 0))))
