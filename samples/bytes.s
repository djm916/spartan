; Helper function to print a bytes object as a sequence of hex values
(defun bytes->hex (bytes)
  (defun byte->hex (b)
    (string-concat "0x" (format-int b 16)))
  (def n (bytes-length bytes))
  (rep ((i 0  (+ 1 i))
        (s "" (string-concat s (byte->hex (bytes-ref bytes i)) " ")))
    (when (= i n) s)))

; Open a file in read-only mode
(def file-name "./samples/test.txt")
(println "Opening " file-name)
(def file (port-open-file file-name '(read)))
; Determine the file's size (in bytes)
(def file-size (port-size file))
; Create a buffer to store the file's contents
(def buffer (make-bytes file-size))
(println "The file size is " file-size " bytes.")
; Read the entire file content into the buffer
(println "Reading file contents...")
(def num-bytes (port-read file buffer 0 (bytes-length buffer)))
(println "Read " num-bytes " bytes from the file.")
; Display the file contents as (hex) bytes
(println "The file contents (as bytes) are: " (bytes->hex buffer))
; Decode the bytes in the file into a (UTF-8) string and display it
(print "The decoded string content is: ")
(println "\"" (bytes->string buffer 0 num-bytes) "\"")
