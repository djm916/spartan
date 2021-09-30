; Return a list of integers in the range [first, last]

(defun range (first last)
  (letrec ((loop (fun (i acc)
                   (if (> i last) acc
                     (cons i (loop (+ 1 i) acc))))))
    (loop first ())))

(defun list/filter (predicate list)
  (if (= list ()) ()
    (if (predicate (car list))
      (cons (car list) (list/filter predicate (cdr list)))
      (list/filter predicate (cdr list)))))

(defun notFactorOf? (y) (fun (x) (not (= (% x y) 0))))

(defun sieve (n)
  (letrec ((loop (fun (xs)
                   (if (= () xs) ()
                     (let* ((x (car xs))
                            (xs (list/filter (notFactorOf? x) xs)))
                       (cons x (loop xs)))))))
    (loop (range 2 n))))
