
; Example of mutual recursion between functions

(defrec
   (even? (fun (n) 
            (if (= 0 n)
              #true
              (odd? (- n 1)))))
   (odd?  (fun (n)
            (if (= 0 n)
              #false
              (even? (- n 1))))))

(print-line "101 is even? " (even? 101))
