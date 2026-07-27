
; Example of the factorial (!) function, written using both functional and imperative styles

; The mathematical definition of factorial is
;
; 0! = 1
; n! = n * (n - 1)!

; Functional style, using (tail) recursion

(defun fact-rec (n)
  (defun loop (n p)
    (if (= 0 n) p
      (loop (- n 1) (* n p))))
  (loop n 1))

; Imperative style, using loops and assignments

(defun fact-loop (n)
  (if (< n 2) n
    (let ((p 1))
      (while (/= n 0)
        (set! p (* n p))
        (set! n (- n 1)))
      p)))

(defun fact-loop2 (n)
  (rep ([n n (- n 1)]
        [p 1 (* n p)])
    :when (= n 0) p))

(def fact fact-loop2)

(print-line "0! = " (fact 0)) ; = 1
(print-line "10! = " (fact 10)) ; = 3628800
(print-line "20! = " (fact 20)) ; = 2432902008176640000
