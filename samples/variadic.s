
; Example of a "sum" function that computes the sum
; of all its arguments, similar to "+". Makes use of
; variadic procedures, as well as "apply".

(defun sum (init &xs)
  (if (= &xs ()) init
    (apply sum (cons (+ init (car &xs)) (cdr &xs)))))

(sum 0)
(sum 1 2 3 4 5)
