; Example of stream usage

(load "stdlib/streams.s")

(defun integer-stream (n)
  (stream-cons n (integer-stream (+ 1 n))))

(def nats (stream-take 10 (integer-stream 1)))

(def squares (stream-map (fun (x) (* x x)) nats))

;(def even-squares (stream-filter (fun (x) (= (remainder x 2) 0)) squares))

(print-line "Integers = " (stream->list nats))
(print-line "Squares = " (stream->list squares))
;(print-line (stream->list (stream-enumerate 1 squares)))
;(print-line "Even squares = " (stream->list even-squares))
;(print-line "Sum of squares = " (stream-reduce + 0 nats))
