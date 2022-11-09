; Example of stream usage

(require "stdlib/streams.s")

(defun int-range (from to)
  (fun ()
    (if (> from to)
      nil
      (let ((next from))
        (set! from (+ 1 from))
        next))))

(def nats (stream (int-range 1 10)))

(def squares (stream/map (fun (x) (* x x)) nats))

(def even-squares (stream/filter (fun (x) (= (remainder x 2) 0)) nats))

(print-line "Integers = " (stream->list nats))
(print-line "Squares = " (stream->list squares))
(print-line "Even squares = " (stream->list even-squares))
(print-line "Sum of squares = " (stream/reduce + 0 nats))
