
; Example of using the "compose" and "->>" macros

(defun square (x) (* x x))
(defun double (x) (* 2 x))

(defun range (lo hi)
  (if (> lo hi) ()
    (adjoin lo (range (+ 1 lo) hi))))
  
(print-line "(2*2)^2 = " ((compose double square) 2))
(print-line "(2*2)^2 = " (->> 2 (double) (square)))

(print-line "2*(2^2) = " ((compose square double) 2))
(print-line "2*(2^2) = " (->> 2 (square) (double)))

(print-line "squares 1..100 = "
  (->> (range 1 100)
       (map square)))

(print-line "sum of squares 1..100 = " (fold-left + 0 (map square (range 1 100))))
