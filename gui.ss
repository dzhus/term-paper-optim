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

(define choice
  (new choice%
       [label "Тестовая функция:"]
       [parent frame] [choices (map test-function-name test-functions)]))

(define (log-poster msg [nl #t])
  (let ((insert (lambda (msg) (send
                          (send log get-editor) insert msg))))
    (insert msg)
    (when nl (insert "\n"))))

(define (run-optimization choice)
  (let* ((f (list-ref test-functions choice))
         (name (test-function-name f))
         (comment (test-function-comment f)))
    (log-poster (format "Оптимизируется ~a" name))
    (when comment
      (log-poster (format "Комментарий: ~a" comment)))
    (log-poster "Протокол работы:")
    (let ((x (optimize (test-function-def f)
                       (test-function-x-start f)
                       #:listener log-poster)))
      (log-poster (format "Ответ: ~a; известное значение минимума: ~a\n"
                          x (test-function-target-x f))))))

(define start
  (new button%
       [label "Пшла!"] [parent frame]
       [callback (lambda (i e)
                   (run-optimization (send choice get-selection)))]
       [stretchable-width #t]))

(send frame show #t)
