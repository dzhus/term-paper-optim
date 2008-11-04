#lang scheme

;;; Functions to maintain and utilize dispatching table for generic
;;; operations

;; Taken from SICP

(require srfi/1)

(provide get put)

;; Dummy dispatching table (really slow)
(define (make-key operation type)
  (cons operation type))

;; Yes, we have one single dispatching table without any way to
;; work with mulitple independent tables at once
(define table '())

;; Empty result should be handled properly in calling function
(define (get operation type)
  (let ((result (assoc (make-key operation type) table)))
    (if result
        (cdr result)
        result)))

(define (put operation type implementation)
  (set! table (alist-cons (make-key operation type)
                          implementation
                          table)))