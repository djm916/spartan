; Example of using the defstruct macro to
; create a simple data type

(require "stdlib/defstruct.s")

(defstruct point (x y))
(def p (point 1 2))
(print-line "p is a point? " (point? p))
(print-line "p = (" (point/x p) ", " (point/y p) ")")
(point/set-x! p 2)
(point/set-y! p 1)
(print-line "p = (" (point/x p) ", " (point/y p) ")")
