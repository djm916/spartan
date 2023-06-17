(require "stdlib/hash-map.s")

; Test example

(defun identity (x) x)

(def h (hash-map identity =))

(hash-map/insert! h 1 "a")
(hash-map/insert! h 2 "b")
(hash-map/insert! h 3 "c")
(hash-map/insert! h 4 "d")
(hash-map/insert! h 5 "e")

(print-line (hash-map/items h))
(print-line (hash-map/keys h))
(print-line (hash-map/values h))
(print-line (hash-map/size h))