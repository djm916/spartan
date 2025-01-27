(load "stdlib/hash-map.s")

(import spartan.hashmap :as hashmap)

; Test example

(def h (hashmap:make-hashmap string-hash =))

(print-line "hashmap? " (hashmap:hashmap? h))

(print-line "An empty hash map: ")
(print-line "size = " (hashmap:size h))
(print-line "empty? " (hashmap:empty? h))

(def elems '(("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5)))

(print-line "Inserting " elems " ...")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (hashmap:insert! h (first pair) (second pair))))

(print-line "Key lookup:")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (if (not (hashmap:contains? h (first pair)))
      (abort "hashmap does not contain expected key!"))
    (print-line (first pair) " => " (hashmap:find h (first pair)))))

(print-line "entries = " (hashmap:entries h))
(print-line "keys = " (hashmap:keys h))
(print-line "values = " (hashmap:values h))
(print-line "size = " (hashmap:size h))

(print-line "Removing all keys...")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (hashmap:remove! h (first pair))))

(print-line "entries = " (hashmap:entries h))
(print-line "keys = " (hashmap:keys h))
(print-line "values = " (hashmap:values h))
(print-line "size = " (hashmap:size h))
