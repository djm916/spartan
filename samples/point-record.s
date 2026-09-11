; Simple example of defining records

(defrecord Point
  point
  point?
  (x point-x point-set-x!)
  (y point-y point-set-y!))
(defun point->string (p)
  (string-concat "(" (format-decimal (point-x p)) ", " (format-decimal (point-y p)) ")"))
(def p (point 0.0 0.0))
(println "p is " p)
(println "p is a point? " (point? p))
(println "(type p) = " (type p))
(println "p = " (point->string p))
(point-set-x! p 1.0)
(point-set-y! p 2.0)
(println "p = " (point->string p))
(match p
  [(record Point a b)
   (println "matched (" a ", " b ")")])
