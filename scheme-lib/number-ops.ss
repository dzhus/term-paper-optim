#lang scheme

;;; Wrapper for built-in number operations

(require "get-put.ss")

(provide install-number-generics-package)

(define (install-number-generics-package)
  (put 'add '(number number) +)
  (put 'mul '(number number) *)
  (put 'reverse '(number) (lambda (x) (/ 1 x))))