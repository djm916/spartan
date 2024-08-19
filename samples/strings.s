(def s "abcdef")

(print-line (string-length s))

(print-line (string-split s ""))
(print-line (string-join "," "a" "b" "c"))

;(print-line (string-delete s (string-find s "c") (string-find s "e")))

; forward iteration using cursor
(let ((end (string-cursor-end s)))
  (for ((i (string-cursor-begin s) (string-cursor-next i)))
    ((= i end) (print-line))
      (print (string-substring s i (string-cursor-next i)))))

; reverse iteration using cursor
(let ((begin (string-cursor-begin s)))
  (for ((i (string-cursor-end s) (string-cursor-prev i)))
    ((= i begin) (print-line))
      (print (string-substring s (string-cursor-prev i) i))))

(print-line (string-insert s "X" (string-cursor-begin s)))
(print-line (string-insert s "X" (string-cursor-end s)))
(print-line (string-insert s "X" (string-find s "d")))

(print-line (string-replace s "c" "Z"))