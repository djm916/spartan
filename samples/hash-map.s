(use spartan.data.hash-map :as map)

; Test example

(def m (map:make-mapping string-hash =))

(print-line "is a mapping? " (map:mapping? m))
(print-line "empty? " (map:empty? m))
(print-line "size = " (map:size m))

(def elems '(("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5)))

(print-line "Inserting key/value pairs...")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (print-line "inserting " (first e))
    (map:insert! m (first pair) (second pair))
    (print-line "keys = " (map:keys m))))

(print-line "size = " (map:size m))

(print-line "Key lookup:")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (if (not (map:contains? m (first pair)))
      (error "mapping does not contain expected key!"))
    (print-line (first pair) " => " (map:find m (first pair)))))

(print-line "Removing all keys...")

(for ((e elems (rest e)))
  ((empty? e) #nil)
  (let ((pair (first e)))
    (print-line "removing " (first pair))
    (map:remove! m (first pair))
    (print-line "keys = " (map:keys m))))

(print-line "size = " (map:size m))
