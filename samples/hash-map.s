(require "stdlib/hash-map.s")

;(import 'spartan.hash-map)

; Test example

(defun identity (x) x)

(def h (spartan.hash-map:hash-map identity =))

(print-line (spartan.hash-map:hash-map? h))
(print-line (spartan.hash-map:size h))
(print-line (spartan.hash-map:empty? h))

(spartan.hash-map:insert! h 1 "a")
(spartan.hash-map:insert! h 2 "b")
(spartan.hash-map:insert! h 3 "c")
(spartan.hash-map:insert! h 4 "d")
(spartan.hash-map:insert! h 5 "e")

(print-line (spartan.hash-map:find h 1))
(print-line (spartan.hash-map:find h 2))
(print-line (spartan.hash-map:find h 3))
(print-line (spartan.hash-map:find h 4))
(print-line (spartan.hash-map:find h 5))

(print-line (spartan.hash-map:entries h))
(print-line (spartan.hash-map:keys h))
(print-line (spartan.hash-map:values h))
(print-line (spartan.hash-map:size h))
