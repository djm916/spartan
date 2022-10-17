
; Example of mutual recursion between functions;
; both global definitions and within a local scope.

(defun even (n)
  (if (= 0 n)
    true
    (odd (- n 1))))

(defun odd (n)
  (if (= 0 n)
    false
    (even (- n 1))))

;(letrec
;  ((even? (fun (n)
;            (if (= 0 n)
;              true
;              (odd? (- n 1)))))
;   (odd?  (fun (n)
;            (if (= 0 n)
;              false
;              (even? (- n 1))))))
;   (even? 11))

(print-line "100 is even? " (even 100))
(print-line "101 is even? " (even 101))
