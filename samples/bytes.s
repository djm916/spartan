(def file (port-open-file "./samples/test.txt" '(read)))
(def file-size (port-size file))
(def buffer (make-bytes file-size))

(defun print-bytes (b)
  (let ((i 0) (n (bytes-length b)))
    (while (< i n)
      (print-line (string-concat "0x" (format-int (bytes-ref b i) 16)))
      (set! i (+ 1 i)))))

(print-line "file is " file-size " bytes.")

(def bytes-read (port-read file buffer 0 (bytes-length buffer)))

(print-line "binary file contents are: ")
(print-bytes buffer)

(print-line "text contents are: ")
(print-line "\"" (bytes->string buffer 0 bytes-read) "\"")
