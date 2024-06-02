
; Examples using variadic functions

; Example of a "sum" function that computes the sum
; of all its arguments, similar to "+". This is NOT
; intended to be good code, but demonstrates the use
; of recursive variadic procedures with "apply".

(defun sum (init & xs)
  (if (null? xs) init
    (apply sum (cons (+ init (car xs)) (cdr xs)))))

(print-line "(sum 0) = " (sum 0))
(print-line "(sum 1 2 3 4 5) = " (sum 1 2 3 4 5))
