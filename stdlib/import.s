(in-package spartan.core)

; <import-statement> => (import <symbol> :as <symbol>)
;                     | (import <symbol> :only (<symbol>+) [:rename ((<symbol> <symbol>)+)])
;                     | (import <symbol> :all [:rename ((<symbol> <symbol>)+)])
;                     | (import <symbol> :except (<symbol>+) [:rename ((<symbol> <symbol>)+)])

(defmacro import (package-name & args)
  (match args
    [(list :as local-alias)
     `(spartan.core:package-alias ',package-name ',local-alias)]
    [(list :all)
     `(spartan.core:%import-all ',package-name ())]
    [(list :all :rename alias-map)
     `(spartan.core:%import-all ',package-name ',alias-map)]
    [(list :only symbols)
     `(spartan.core:%import-only ',package-name ',symbols ())]
    [(list :only symbols :rename alias-map)
     `(spartan.core:%import-only ',package-name ',symbols ',alias-map)]
    [(list :except excludes)
     `(spartan.core:%import-except ',package-name ',excludes ())]
    [(list :except excludes :rename alias-map)
     `(spartan.core:%import-except ',package-name ',excludes ',alias-map)]))

(defun %import (package symbols alias-map)
  (defun lookup (symbol)
    (let ((entry (find (fun (pair) (= symbol (first pair))) alias-map)))
      (if (nil? entry) symbol (second entry))))
  (for-each
    (fun (symbol) (package-bind (lookup symbol) (package-resolve symbol package)))
    symbols))

(defun %import-only (package-name symbols alias-map)
  (%import (the-package package-name) symbols alias-map))

(defun %import-all (package-name alias-map)
  (let* ((package (the-package package-name))
         (symbols (package-symbols package)))
    (%import package symbols alias-map)))

(defun %import-except (package-name excludes alias-map)
  (let* ((package (the-package package-name))
         (symbols (remove (fun (s) (contains? s excludes))
                          (package-symbols package))))
    (%import package symbols alias-map)))
