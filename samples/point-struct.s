; Example of using the defstruct macro

(defstruct point (x y))
(def p (make-point 1 2))
(print-line "p is " p)
(print-line "p is a point? " (point? p))
(print-line "p = (" (p 'x) ", " (p 'y) ")")
(set! (p 'x) 2)
(set! (p 'y) 1)
(print-line "p = (" (p 'x) ", " (p 'y) ")")
