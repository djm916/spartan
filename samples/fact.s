
; Example of the factorial (!) function, written using both functional
; and imperative styles, as well as arbitrary precision integers.

; The mathematical definition of factorial is:
;
; 0! = 1
; n! = n * (n - 1)!

; Functional style, using (tail) recursion

(defun fact-rec (n)
  (defun fact-rect (n p)
    (if (= 0 n) p
      (fact-rect (- n 1) (* n p))))
  (fact-rect n 1))

;Imperative style, with loops and assignments

(defun fact-loop (n)
  (if (< n 2) n
    (let ((p 1))
      (while (/= n 0)
        (set! p (* n p))
        (set! n (- n 1)))
      p)))

(print-line "6! = " (fact-rec 6))
(print-line "6! = " (fact-loop 6))
