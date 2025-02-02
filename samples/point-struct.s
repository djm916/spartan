; Simple example of defining records

(defrecord point (x y))
(defun point->string (p)
  (string-concat "(" (format-decimal (point-x p)) ", " (format-decimal (point-y p)) ")"))
(def p (make-point 0.0 0.0))
(print-line "p is " p)
(print-line "p is a point? " (point? p))
(print-line "(type p) = " (type p))
(print-line "p = " (point->string p))
(set-point-x! p 1.0)
(set-point-y! p 2.0)
(print-line "p = " (point->string p))
(match p
  [(record point a b)
   (print-line "matched point (" a ", " b ")")])
