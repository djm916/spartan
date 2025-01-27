(in-package spartan.core)

; <import-statement> => (import <package> :as <alias>)
;                     | (import <package> <import-specifier>+)
;                     | (import <package> :all)
; <import-specifier> => <symbol> (:as <alias>)?
; <package> => <symbol>
; <alias> => <symbol>

(defmacro import (package-name & args)
  (match args
    [(list :as local-alias)
     `(spartan.core:package-alias ',package-name ',local-alias)]
    [(list :all)
     `(spartan.core:%import-all ',package-name)]
    [_
     `(spartan.core:%import-only ',package-name ',(%parse-specifiers args))]))

(defun %parse-specifiers (specifiers)
  (match specifiers
    [(list* symbol :as alias ...rest)
     (adjoin (list symbol alias) (%parse-specifiers ...rest))]
    [(list* symbol ...rest)
     (adjoin (list symbol symbol) (%parse-specifiers ...rest))]
    [(list)
     ()]))

(defun %import-only (package-name specifiers)
  (let ((from-pkg (the-package package-name)))
    (for ((spec specifiers (rest spec)))
      ((empty? spec) #nil)
      (let-values (((symbol alias) (first spec)))
        (package-bind alias (package-resolve symbol from-pkg))))))
