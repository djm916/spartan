(use spartan.hash-map :as map)

; Test example

(def m (map:make-mapping string-hash =))

(print-line "is a mapping? " (map:mapping? m))
(print-line "empty? " (map:empty? m))
(print-line "size = " (map:size m))

(def elems '(("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5)))

(print-line "Inserting " elems " ...")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (map:insert! m (first pair) (second pair))))

(print-line "Key lookup:")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (if (not (map:contains? m (first pair)))
      (error "mapping does not contain expected key!"))
    (print-line (first pair) " => " (map:find m (first pair)))))

(print-line "entries = " (map:entries m))
(print-line "keys = " (map:keys m))
(print-line "values = " (map:values m))
(print-line "size = " (map:size m))

(print-line "Removing all keys...")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (map:remove! m (first pair))))

(print-line "entries = " (map:entries m))
(print-line "keys = " (map:keys m))
(print-line "values = " (map:values m))
(print-line "size = " (map:size m))
