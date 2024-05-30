; Simple example of defining records

(defrecord point (x y))
(defun point->string (p)
  (string-concat "(" (format-decimal (point-x p)) ", " (format-decimal (point-y p)) ")"))
(def p (make-point 0.0 0.0))
(print-line "p is " p)
(print-line "p is a point? " (point? p))
(print-line "p = " (point->string p))
(point-set-x! p 1.0)
(point-set-y! p 2.0)
(print-line "p = " (point->string p))
