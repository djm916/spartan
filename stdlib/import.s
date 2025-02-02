(in-ns spartan.core)

; <import-statement> => (import <symbol> :as <symbol>)
;                     | (import <symbol> :only (<symbol>+) [:rename ((<symbol> <symbol>)+)])
;                     | (import <symbol> :all [:rename ((<symbol> <symbol>)+)])
;                     | (import <symbol> :except (<symbol>+) [:rename ((<symbol> <symbol>)+)])

(defmacro import (ns-name & args)
  (match args
    [(list :as local-alias)
     `(spartan.core:ns-alias ',ns-name ',local-alias)]
    [(list :all)
     `(spartan.core:%import-all ',ns-name ())]
    [(list :all :rename alias-map)
     `(spartan.core:%import-all ',ns-name ',alias-map)]
    [(list :only symbols)
     `(spartan.core:%import-only ',ns-name ',symbols ())]
    [(list :only symbols :rename alias-map)
     `(spartan.core:%import-only ',ns-name ',symbols ',alias-map)]
    [(list :except excludes)
     `(spartan.core:%import-except ',ns-name ',excludes ())]
    [(list :except excludes :rename alias-map)
     `(spartan.core:%import-except ',ns-name ',excludes ',alias-map)]))

(defun %import (ns symbols alias-map)
  (defun lookup (symbol)
    (let ((entry (find (fun (pair) (= symbol (first pair))) alias-map)))
      (if (nil? entry) symbol (second entry))))
  (for-each
    (fun (symbol) (ns-bind (lookup symbol) (ns-resolve symbol ns)))
    symbols))

(defun %import-only (ns-name symbols alias-map)
  (%import (the-ns ns-name) symbols alias-map))

(defun %import-all (ns-name alias-map)
  (let* ((ns (the-ns ns-name))
         (symbols (ns-symbols ns)))
    (%import ns symbols alias-map)))

(defun %import-except (ns-name excludes alias-map)
  (let* ((ns (the-ns ns-name))
         (symbols (remove (fun (s) (contains? s excludes))
                          (ns-symbols ns))))
    (%import ns symbols alias-map)))
