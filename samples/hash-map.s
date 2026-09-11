(use spartan.data.hash-map :as map)

(def m (map:mapping string-hash =))

(println "is a mapping? " (map:mapping? m))
(println "empty? " (map:empty? m))
(println "size = " (map:size m))

(def entries '(("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5)))

(println "Inserting key/value pairs...")

(foreach
  (fun (entry)  
    (println "inserting " entry)
    (map:insert! m (first entry) (second entry))
    (println "keys = " (map:keys m)))
  entries)

(println "size = " (map:size m))

(println "Key lookup:")

(foreach
  (fun (entry)
    (if (not (map:contains? m (first entry)))
      (error "mapping does not contain expected key!"))
    (println (first entry) " => " (map:find m (first entry))))
  entries)

(println "Removing all keys...")

(foreach
  (fun (entry)
    (println "removing " (first entry))
    (map:remove! m (first entry))
    (println "keys = " (map:keys m)))
  entries)

(println "size = " (map:size m))
