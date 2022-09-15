
; Example of using the "compose" and "pipe" macros

(defun square (x) (* x x))
(defun double (x) (* 2 x))

(print-line "(2*2)^2 = " ((compose double square) 2))
(print-line "(2*2)^2 = " (pipe 2 double square))
(print-line "2*(2^2) = " (pipe 2 square double))
(print-line "2*(2^2) = " ((compose square double) 2))
