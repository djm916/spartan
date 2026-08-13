
; To run with JFR
; java --enable-preview -XX:StartFlightRecording:filename=./jfr_records/stream-test2.jfr -jar ./Spartan.jar ./samples/stream-test2.s

(defun integer-stream (n)
  (stream-adjoin n (integer-stream (+ 1 n))))

(defun times3 (n)
  (stream-ref 3
    (stream-filter
      (fun (x) (= 0 (remainder x n)))
      (integer-stream 0))))

(def N 2000000)
(print-line (times3 N))
