(define even
  (fun (n)
    (if (= 0 n)
      true
      (odd (- n 1)))))

(define odd
  (fun (n)
    (if (= 0 n)
      false
      (even (- n 1)))))
