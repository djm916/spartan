; Example of using the defstruct macro to
; create a simple data type

(defstruct point (x y))
(def p (make-point 1 2))
(print-line "p is a point? " (point? p))
(print-line "p = (" (point-x p) ", " (point-y p) ")")
(set-point-x! p 2)
(set-point-y! p 1)
(print-line "p = (" (point-x p) ", " (point-y p) ")")
