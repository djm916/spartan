(use spartan.data.hash-map :as map)

(def m (map:make-mapping string-hash =))

(print-line "is a mapping? " (map:mapping? m))
(print-line "empty? " (map:empty? m))
(print-line "size = " (map:size m))

(def entries '(("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5)))

(print-line "Inserting key/value pairs...")

(foreach
  (fun (entry)  
    (print-line "inserting " entry)
    (map:insert! m (first entry) (second entry))
    (print-line "keys = " (map:keys m)))
  entries)

(print-line "size = " (map:size m))

(print-line "Key lookup:")

(foreach
  (fun (entry)
    (if (not (map:contains? m (first entry)))
      (error "mapping does not contain expected key!"))
    (print-line (first entry) " => " (map:find m (first entry))))
  entries)

(print-line "Removing all keys...")

(foreach
  (fun (entry)
    (print-line "removing " (first entry))
    (map:remove! m (first entry))
    (print-line "keys = " (map:keys m)))
  entries)

(print-line "size = " (map:size m))
