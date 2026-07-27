(in-module spartan.base)

; <import-form> => (import <module-name> :as <alias>)
;                | (import <module-name> :all <alias-map>?)
;                | (import <module-name> :only (<symbol>+) <alias-map>?)
;                | (import <module-name> :except (<symbol>+) <alias-map>?)
; <alias-map> => :rename ((<symbol> <symbol>)+)

(export use import %import-all %import-only %import-except)

(defmacro use (module-name & args)
  `(do (spartan.base:load ,(module-name->path module-name))
       (spartan.base:import ,module-name ,@args)))

(defmacro import (module-name & args)
  (match args
    [(list :as local-alias)
     `(spartan.base:module-alias ',module-name ',local-alias)]
    [(list :all)
     `(spartan.base:%import-all ',module-name ())]
    [(list :all :rename alias-map)
     `(spartan.base:%import-all ',module-name ',alias-map)]
    [(list :only symbols)
     `(spartan.base:%import-only ',module-name ',symbols ())]
    [(list :only symbols :rename alias-map)
     `(spartan.base:%import-only ',module-name ',symbols ',alias-map)]
    [(list :except excludes)
     `(spartan.base:%import-except ',module-name ',excludes ())]
    [(list :except excludes :rename alias-map)
     `(spartan.base:%import-except ',module-name ',excludes ',alias-map)]))

(defun %import (module symbols alias-map)
  (defun lookup-alias (symbol)
    (let ((entry (find (fun (pair) (= symbol (first pair))) alias-map)))
      (if (nil? entry) symbol (second entry))))
  (foreach
    (fun (symbol) (module-import module symbol (lookup-alias symbol)))
    symbols))

(defun %import-only (module-name symbols alias-map)
  (%import (the-module module-name) symbols alias-map))

(defun %import-all (module-name alias-map)
  (let* ((module (the-module module-name))
         (symbols (module-symbols module)))
    (%import module symbols alias-map)))

(defun %import-except (module-name excludes alias-map)
  (let* ((module (the-module module-name))
         (symbols (remove (fun (s) (contains? s excludes))
                          (module-symbols module))))
    (%import module symbols alias-map)))
