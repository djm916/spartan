(load "stdlib/list.txt")

(defun not-factor? (y) (fun (x) (not (= (% x y) 0))))

(defun prime-sieve (n)
  (defun loop (xs)
    (if (= () xs) ()
      (let* ((x (car xs))
             (xs (list/filter (not-factor? x) xs)))
        (cons x (loop xs)))))
  (loop (list/range 2 n)))
