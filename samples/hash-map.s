(load "stdlib/hash-map.s")

(import 'spartan.hash-map :as 'hmap)

; Test example

(def h (hmap:make-hash-map string-hash =))

(print-line "hashmap? " (hmap:hash-map? h))
(print-line "size = " (hmap:size h))
(print-line "empty? " (hmap:empty? h))

(hmap:insert! h "a" 1)
(hmap:insert! h "b" 2)
(hmap:insert! h "c" 3)
(hmap:insert! h "d" 4)
(hmap:insert! h "e" 5)

(print-line "a => " (hmap:find h "a"))
(print-line "b => " (hmap:find h "b"))
(print-line "c => " (hmap:find h "c"))
(print-line "d => " (hmap:find h "d"))
(print-line "e => " (hmap:find h "e"))

(print-line (hmap:entries h))
(print-line (hmap:keys h))
(print-line (hmap:values h))
(print-line (hmap:size h))
