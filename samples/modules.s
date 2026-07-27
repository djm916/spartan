
(in-module a)
(def x 1)
(def y 2)
(export x)

(in-module b)
(print-line "a:x=" a:x)
;(a:y) ; y is not exported from module a; raise an error

(import a :only (x))
(export x)

(in-module c)
;(print-line "b:x=" b:x) ; x is not exported from module b; raise an error
(set! b:x 11)
(print-line "b:x=" b:x)
(print-line "a:x=" a:x)
