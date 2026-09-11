(def s "abcdef")

(println (string-length s))

(println (string-split s ""))
(println (string-join "," "a" "b" "c"))

;(println (string-delete s (string-find s "c") (string-find s "e")))

; forward iteration using cursor
(let ((end (string-cursor-end s)))
  (for ((i (string-cursor-begin s) (string-cursor-next i)))
    ((= i end) (println))
      (print (string-substring s i (string-cursor-next i)))))

; reverse iteration using cursor
(let ((begin (string-cursor-begin s)))
  (for ((i (string-cursor-end s) (string-cursor-prev i)))
    ((= i begin) (println))
      (print (string-substring s (string-cursor-prev i) i))))

(println (string-insert s "X" (string-cursor-begin s)))
(println (string-insert s "X" (string-cursor-end s)))
(println (string-insert s "X" (string-find s "d")))

(println (string-replace s "c" "Z"))