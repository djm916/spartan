
; Example of the factorial (!) function, written using both functional
; and imperative styles, as well as arbitrary precision integers.

; The mathematical definition of factorial is:
;
; 0! = 1
; n! = n * (n - 1)!

; Functional style, using (tail) recursion

(defun fact-rec (n)
  (defun fact-rect (n p)
    (if (= 0L n) p
      (fact-rect (- n 1L) (* n p))))
  (fact-rect n 1L))

;Imperative style, with loops and assignments

(defun fact-loop (n)
  (if (< n 2L) n
    (let ((p 1L))
      (while (/= n 0L)
        (set! p (* n p))
        (set! n (- n 1L)))
      p)))

(print-line "6! = " (fact-rec 6L))
(print-line "17! = " (fact-rec 17L))
(print-line "6! = " (fact-loop 6L))
(print-line "17! = " (fact-loop 17L))
