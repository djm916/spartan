; Examples of stream usage

(load "stdlib/streams.txt")

(defun int-range (from to)
  (fun ()
    (if (> from to)
      nil
      (let ((next from))
        (set! from (+ 1 from))
        next))))

(def s1 (stream/new (int-range 1 10)))

(def s2 (stream/map (fun (x) (* x x)) s1))

(def s3 (stream/filter (fun (x) (= (% x 2) 0)) s1))

(defun print-stream (s)
  (while (not (stream/empty? s))
    (printnl (stream/car s))
    (set! s (stream/cdr s))))

(print-stream s1)
(print-stream s2)
(print-stream s3)
(printnl (stream/reduce + 0 s1))
