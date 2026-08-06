
(defun even? (n)
  (= 0 (remainder n 2)))

(defun double (n)
  (* 2 n))

(defun integer-stream (n)
  (stream-adjoin n (integer-stream (+ 1 n))))

(defun consume-stream (stream count)
  (cond
    ((= 0 count)
     (print-line "Test complete. No crash!\n"))
    ((stream-empty? stream) 
     (print-line "Stream ended unexpectedly.\n"))
    (else
     ;; Passing (stream-rest stream) directly allows the old head 
     ;; to be garbage collected on each tail-recursive step.
     (consume-stream (stream-rest stream) (- count 1)))))

; Executing the test this way, retaining a reference to the head of the stream,
; causes a memory leak!
; (let ((my-stream (integer-stream 0)))
;   (print-line "Starting memory leak test...\n")
;   (consume-stream my-stream 10000000))

(print-line "Starting memory leak test...\n")
(consume-stream (stream-filter even? (stream-map double (integer-stream 0))) 1000000)
