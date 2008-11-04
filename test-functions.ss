#lang scheme

;;; Test functions from «Applied Nonlinear Programming» by
;;; D. Himmelblau

(provide (rename-out [d:make-test-function make-test-function])
         test-function-def
         test-function-x-start
         test-function-target-x
         test-function-target-value
         test-function-comment)

;; TODO Support several extremal points
(define-struct test-function (def
                              x-start
                              target-x target-value
                              comment))

(define (d:make-test-function def x-start
                         target-x target-value
                         [comment #f])
  (make-test-function def x-start
                      target-x target-value
                      comment))

;; Rosenbrock function
(define test-functions
  (list
   (d:make-test-function
    (lambda (x1 x2)
      (+ (* 100 (sqr (- x2 (sqr x1))))
         (sqr (- 1 x1))))
    '#(-1.2 1)
    '#(1 1) 0 "Тест Розенброка")

   (d:make-test-function
    (lambda (x1 x2)
      (+ (sqr (- x2 (sqr x1)))
         (sqr (- 1 x1))))
    '#(-1.2 1)
    '#(1 1) 0)

   (d:make-test-function
    (lambda (x1 x2)
      (+ (sqr (- x2 (sqr x1)))
         (* 100 (sqr (- 1 x1)))))
    '#(-1.2 1)
    '#(1 1) 0)
   
   (d:make-test-function
    (lambda (x1 x2)
      (+ (sqr (- (+ (sqr x1) x2) 11))
         (sqr (- (+ x1 (sqr x2)) 7))))
    '#(1 1)
    '#(3 2) 0 (format "Есть доп. минимум в точке ~a" '#(3.58 -1.84)))))