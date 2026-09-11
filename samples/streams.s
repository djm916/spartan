; Example of stream usage

(defun integer-stream (n)
  (stream-adjoin n (integer-stream (+ 1 n))))

(def nats (stream-take 10 (integer-stream 1)))

(def squares (stream-map (fun (x) (* x x)) nats))

(defun even? (n) (= (remainder n 2) 0))

(def even-squares (stream-filter even? squares))

(println "Integers = " (stream->list nats))
(println "length = " (stream-length nats))
(println "Squares = " (stream->list squares))
(println "Even Squares = " (stream->list even-squares))
(println "Sum of squares = " (stream-reduce + 0 squares))
