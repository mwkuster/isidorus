(defun print-line(param)
	(format t "~a~%" param))
	
	
(defun add(a b)
	(+ a b))
	


(let ((line-str (concatenate 'string "the result of 6 + 2 is " (write-to-string (add 6 2)))))
	(print-line line-str))