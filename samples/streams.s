; Examples of stream usage

(require "stdlib/streams.s")

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

(print-line "Integers from 1 to 10 = " (stream->list s1))
(print-line "Squares up to 10 = " (stream->list s2))
(print-line "Even squares = " (stream->list s3)
(print-line "Sum of integers up to 10 = " (stream/reduce + 0 s1))
(print-line "Sum of squares up to 10 = " (stream/reduce + 0 s2))
