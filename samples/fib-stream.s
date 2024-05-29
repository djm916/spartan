
(defun sum-streams (s1 s2)
  (stream-cons (+ (stream-car s1) (stream-car s2))
               (sum-streams (stream-cdr s1) (stream-cdr s2))))

; fibs                                  | 0 1 1 2 3 5 ...
; (stream-cdr fibs)                     | 1 1 2 3 5 ...
; (sum-streams fibs (stream-cdr fibs))  | 1 2 3 5 ...

(def fibs (stream-cons 0 (stream-cons 1 (sum-streams fibs (stream-cdr fibs)))))

(def N 10)

(def fibs-to-N (stream->list (stream-take N fibs)))

(print-line "The first " N " Fibonacci numbers are: " fibs-to-N)
