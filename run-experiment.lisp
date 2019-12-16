(defun print-n-eval (n)
  (if (= 0 n)
    nil
    (progn
      (format t "~S,~F,~F~C"
              ###-formula-placeholder-###
              *sigma* *gamma* #\linefeed)
      (print-n-eval (- n 1)))))

(setf *sigma* 1)
(setf *gamma* 1)
(print-n-eval 100)

(setf *sigma* 1)
(setf *gamma* 0)
(print-n-eval 100)

(setf *sigma* 0)
(setf *gamma* 1)
(print-n-eval 100)

(setf *sigma* 0)
(setf *gamma* 0)
(print-n-eval 100)

(quit)

