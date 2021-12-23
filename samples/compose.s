
; Example of using the "compose" and "pipe" macros

(defun square (x) (* x x))
(defun double (x) (* 2 x))

((compose double square) 2)

(pipe 2 double square)
(pipe 2 double double square)
(pipe 2 double square square)