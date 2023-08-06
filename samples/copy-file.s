
(def file (port/open "./samples/test.txt" "r"))
(def buffer (bytes/make 1024))

(let ((eof false))
  (while (not eof)
    (let ((bytes-read (port/read file buffer 0 3)))
      (set! eof (< bytes-read 0))
      (if (not eof)
        (port/write port/stdout buffer 0 bytes-read)))))

(port/close file)
