
(def file (port-open-file "./samples/test.txt" '(read)))
(def buffer (make-bytes 1024))

(let ((eof false))
  (while (not eof)
    (let ((bytes-read (port-read file buffer 0 3)))
      (set! eof (< bytes-read 0))
      (if (not eof)
        (port-write standard-output-port buffer 0 bytes-read)))))

(port-close file)

