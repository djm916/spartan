
; Define Fibonacci sequence as an iterative procedure

(defun fib (n)
  (for  ((a 0 b)
         (b 1 (+ a b))
         (n n (- n 1)))
        ((= n 0) a)))

; Display first 50 Fibonacci numbers
(for ((i 0 (+ i 1)))
     ((> i 50) nil)
  (print-line "(fib " i ") = " (fib i)))
