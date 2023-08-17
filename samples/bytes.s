(def file (port:open "./samples/test.txt" "rw"))
(def file-size (port:length file))
(def buffer (bytes:new file-size))

(defun print-bytes (b)
  (let ((i 0) (n (length b)))
    (while (< i n)
      (print-line (string:concat "0x" (format-int (b i) 16)))
      (set! i (+ 1 i)))))

(print-line "file is " file-size " bytes.")

(def bytes-read (port:read file buffer 0 (length buffer)))

(print-line "binary file contents are: ")
(print-bytes buffer)

(print-line "text contents are: ")
(print-line "\"" (bytes->string buffer 0 bytes-read) "\"")
