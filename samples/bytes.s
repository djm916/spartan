(def file (port/open "./samples/test.txt" "r"))
(def buffer (bytes/new 20))

(defun print-bytes (b)
  (let ((i 0) (n (length b)))
    (while (< i n)
      (print-line (string/concat "0x" (format-int (at b i) 16)))
      (set! i (+ 1 i)))))

(def bytes-read (port/read file buffer 0 (length buffer)))
(print-bytes buffer)
(print-line "\"" (bytes->string buffer 0 bytes-read) "\"")
