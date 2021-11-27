
(load "stdlib/streams.s")

(defun not-factor? (y) (fun (x) (not (= (% x y) 0))))

(defun int-range (from to)
  (fun ()
    (if (> from to)
      nil
      (let ((next from))
        (set! from (+ 1 from))
        next))))
        
(stream/reduce cons () (stream/new (int-range 1 10)))

