
(defun make-fib-generator ()
  (let ((a 0) (b 1))    
    (fun ()
      (let ((sum (+ a b)))
        (set! a b)
        (set! b sum)
        sum))))

(def fib-stream (stream-cons 0 (stream-cons 1 (make-stream (make-fib-generator)))))

(def N 10)

(def fibs-to-N (stream->list (stream-take N fib-stream)))

(print-line "The first " N " Fibonacci numbers are: " fibs-to-N)
