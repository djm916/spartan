(def b (bytes/new 3))

(def print-bytes (fun (b)
  (while (not (empty? b))
    (print-line (bytes/pop! b)))))

(print-bytes b)
