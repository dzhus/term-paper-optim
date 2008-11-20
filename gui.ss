#lang scheme/gui

(require "relch.ss"
         "test-functions.ss")

(define frame
  (new frame%
       [label "Демонстрация метода оптимизации с чебышёвской функцией релаксации"]
       [width 500] [height 500]
       [alignment '(left top)]))

(define log
  (new text-field%
       [label "Протокол работы:"] [parent frame] [style '(multiple vertical-label)]))

(define control-pane
  (new vertical-pane%
       [parent frame]
       [stretchable-height #f]
       [stretchable-width #t]))

(define choice
  (new choice%
       [label "Тестовая функция:"]
       [parent control-pane]
       [choices (map (lambda (e) (test-function-name (cdr e))) test-functions)]))

(define eps
  (new slider%
       [label "Точность"]
       [parent control-pane]
       [min-value 1]
       [max-value 10]))

(define iterations
  (new slider%
       [label "Максимум итераций"]
       [parent control-pane]
       [min-value 10]
       [max-value 10000]))

(define degree
  (new slider%
       [label "Степень функции релакации L"]
       [parent control-pane]
       [min-value 5]
       [max-value 20]))

(define (log-poster msg [nl #t])
  (let ((insert (lambda (msg) (send
                          (send log get-editor) insert msg)))
        (msg (if (string? msg) msg (format "~a" msg))))
    (insert msg)
    (when nl (insert "\n"))))

(define (run-optimization function-idx)
  (let* ((f (cdr (list-ref test-functions function-idx)))
         (name (test-function-name f))
         (comment (test-function-comment f)))
    (log-poster (format "Оптимизируется ~a" name))
    (when comment (log-poster (format "Комментарий: ~a" comment)))
    (log-poster "Протокол работы:")
    (let ((x (relch-optimize (test-function-def f)
                             (test-function-x-start f)
                             #:eps (expt 10 (- (send eps get-value)))
                             #:iterations (send iterations get-value)
                             #:degree (send degree get-value)
                             #:listener log-poster)))
      (log-poster (format "Ответ: ~a; известное значение минимума: ~a\n"
                          x (test-function-target-x f))))))

(define start
  (new button%
       [label "Пшла!"] [parent control-pane]
       [callback (lambda (i e)
                   (run-optimization (send choice get-selection)))]
       [stretchable-width #t]))

(send frame show #t)
