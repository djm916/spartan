
; Example of the factorial (!) function, written using both functional
; and imperative styles.

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

(print-line "10! = " (fact-loop 10)) ; should equal 3628800
(print-line "20! = " (fact-loop 20)) ; should equal 2432902008176640000
