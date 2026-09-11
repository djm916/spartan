
; Example of using the "compose" and "->>" (threading) macros

(defun square (x) (* x x))
(defun double (x) (* 2 x))

(defun range (lo hi)
  (if (> lo hi) ()
    (adjoin lo (range (+ 1 lo) hi))))
  
(println "(2*2)^2 = " ((compose double square) 2))
(println "(2*2)^2 = " (->> 2 (* 2) (square)))

(println "2*(2^2) = " ((compose square double) 2))
(println "2*(2^2) = " (->> 2 (square) (* 2)))

(println "squares 1..100 = "
  (->> (range 1 100)
       (map square)))

(println "sum of squares 1..100 = " (fold-left + 0 (map square (range 1 100))))
