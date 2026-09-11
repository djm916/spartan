(in-module spartan.base)

(export cons pair? car cdr set-car! set-cdr!)

(defrecord Pair
  cons
  pair?
  (car car set-car!)
  (cdr cdr set-cdr!))
