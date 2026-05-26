
(in-module a)
(def x 1)
(def y 2)
(export x)

(in-module b)
(print-line "a:x=" a:x)
(a:y) ; y is not exported from module a; raise an error

(import a :only (x))
x
