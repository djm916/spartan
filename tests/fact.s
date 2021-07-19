(defun fact-rec (n)
  (letrec ((loop (fun (n p)
                   (if (= 0 n)
                     p
                     (loop (- n 1) (* n p))))))
    (loop n 1)))

(defun fact-loop (n)
  (if (< n 2)
    n
    (let ((p 1))
      (while (> n 0)
        (set! p (* n p))
        (set! n (- n 1)))
      p)))
