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
       [label "Степень функции релаксации L"]
       [parent control-pane]
       [min-value 5]
       [max-value 200]))

;; Post a string or clear log area
(define (log-poster raw-msg [nl #t])
  (let* ((editor (send log get-editor))
         (insert (lambda (string) (send editor insert string))))
    (if (not (eq? raw-msg 'clear))
        (insert (if (string? raw-msg) raw-msg (format "~a" raw-msg)))
        ;; Finally I have something really gay in mah beautiful Scheme
        ;; source:
        (begin
          (send editor select-all)
          (send editor clear)))
    (when nl (insert "\n"))))

(define (run-optimization function-idx)
  (let* ((f (cdr (list-ref test-functions function-idx)))
         (name (test-function-name f))
         (comment (test-function-comment f)))
    (log-poster 'clear)
    (log-poster (format "Оптимизируется ~a" name))
    (when comment (log-poster (format "Комментарий: ~a" comment)))
    (log-poster "Протокол работы:")
    (let ((x (relch-optimize (test-function-def f)
                             (test-function-x-start f)
                             (expt 10 (- (send eps get-value)))
                             (send iterations get-value)
                             (send degree get-value)
                             log-poster)))
      (log-poster (format "Ответ: ~a; Известные точки минимума: ~a\n"
                          x (test-function-target-x f))))))

(define start
  (new button%
       [label "Пшла!"] [parent control-pane]
       [callback (lambda (i e)
                   (run-optimization (send choice get-selection)))]
       [stretchable-width #t]))

(send frame show #t)
