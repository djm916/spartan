; Example of stream usage

(defun integer-stream (n)
  (stream-cons n (integer-stream (+ 1 n))))

(def nats (stream-take 10 (integer-stream 1)))

(def squares (stream-map (fun (x) (* x x)) nats))

(print-line "Integers = " (stream->list nats))
(print-line "Squares = " (stream->list squares))
(print-line "Sum of squares = " (stream-reduce + 0 squares))
(print-line "Random numbers = " (stream->list (stream-take 10 (generator->stream rand))))
