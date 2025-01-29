
(defun sum-streams (s1 s2)
  (stream-adjoin (+ (stream-first s1) (stream-first s2))
                 (sum-streams (stream-rest s1) (stream-rest s2))))

; fibs                                  | 0 1 1 2 3 5 ...
; (stream-rest fibs)                    | 1 1 2 3 5 ...
; (sum-streams fibs (stream-rest fibs)) | 1 2 3 5 ...

(def fibs (stream-adjoin 0 (stream-adjoin 1 (sum-streams fibs (stream-rest fibs)))))

(def N 10)

(def fibs-to-N (stream->list (stream-take N fibs)))

(print-line "The first " N " Fibonacci numbers are: " fibs-to-N)
