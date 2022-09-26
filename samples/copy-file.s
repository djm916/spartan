
(def file (port/open "./samples/test.txt" "r"))
(def buffer (bytes/new 3))

(let ((eof false))
  (while (not eof)
    (let ((bytes-read (port/read file buffer)))
      (set! eof (< bytes-read 0))
      (if (not eof)
        (do (bytes/flip! buffer)
            (port/write port/stdout buffer)
            (bytes/clear! buffer))))))
