
(defun test ()
  (raise (make-exception 'some-exception "some exception")))

(guard (test)
  (some-exception (print-line "some exception occurred")))
