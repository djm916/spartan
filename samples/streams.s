; Example of stream usage

(load "stdlib/streams.s")

(defun integer-stream (n)
  (stream-cons n (integer-stream (+ 1 n))))

(def nats (stream-take 10 (integer-stream 1)))

(def squares (stream-map (fun (x) (* x x)) nats))

(print-line "Integers = " (stream->list nats))
(print-line "Squares = " (stream->list squares))
(print-line "Sum of squares = " (stream-reduce + 0 squares))
