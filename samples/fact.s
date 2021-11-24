
; Example of the factorial (!) function, written
; using both functional and imperative styles.

; 0! = 1
; n! = n * (n - 1)!

; This is the functional style, using tail-recursion

(defun fact-rec (n)
  (defun loop (n p)
    (if (= 0 n) p
      (loop (- n 1) (* n p))))
  (loop n 1))

; This is the imperative style, with assignment
; and a loop

(defun fact-loop (n)
  (if (< n 2) n
    (let ((p 1))
      (while (> n 0)
        (set! p (* n p))
        (set! n (- n 1)))
      p)))

(fact-rec 6)
(fact-loop 6)
