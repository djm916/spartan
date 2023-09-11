(require "stdlib/hash-map.s")

(import 'spartan.hash-map :as 'hmap)

; Test example

(defun identity (x) x)

(def h (hmap:hash-map identity =))

(print-line (hmap:hash-map? h))
(print-line (hmap:size h))
(print-line (hmap:empty? h))

(hmap:insert! h 1 "a")
(hmap:insert! h 2 "b")
(hmap:insert! h 3 "c")
(hmap:insert! h 4 "d")
(hmap:insert! h 5 "e")

(print-line (hmap:find h 1))
(print-line (hmap:find h 2))
(print-line (hmap:find h 3))
(print-line (hmap:find h 4))
(print-line (hmap:find h 5))

(print-line (hmap:entries h))
(print-line (hmap:keys h))
(print-line (hmap:values h))
(print-line (hmap:size h))
