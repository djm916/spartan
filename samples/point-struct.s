(load "stdlib/defstruct.s")

; Example of using defstruct macro

(defstruct point (x y))
(def p (make-point 1 2))
(point-x p)
(point-y p)
(set-point-x! p 2)
(set-point-y! p 1)
(point? p)

