(in-package spartan.core)

; 

(defun %do-import (package-name local-alias specifiers)
  (if (not (nil? local-alias))
	  (package-alias package-name local-alias)
    (let* ((from-pkg (the-package package-name))
           (specifiers (if (empty? specifiers)
                         (map (fun (s) (list s s)) (package-symbols from-pkg))
                         specifiers)))
      (for ((spec specifiers (rest spec)))
        ((empty? spec) #nil)
        (let-values (((symbol alias) (first spec)))
          (package-bind alias (package-resolve symbol from-pkg)))))))

; <import-statement> => (import <package> :as <alias>)
;                     | (import <package> <import-specifier>+)
;                     | (import <package> :all)
; <import-specifier> => <symbol> (:as <alias>)?
; <package> => <symbol>
; <alias> => <symbol>

(defmacro import (package-name & args)
  (let ((specifiers ())
        (local-alias #nil))    
    
    ; parse optional local package alias
    (if (and (not (empty? args)) (= (first args) :as))
      (do
        (set! args (rest args))
        (if (empty? args)
          (abort "malformed expression"))
        (set! local-alias (first args))
        (set! args (rest args))))
    
    ; parse imported symbols with optional aliases
    (while (not (empty? args))
      (let* ((symbol (first args))
             (alias symbol))
        (if (and (not (empty? (rest args))) (= (second args) :as))
          (do
            (set! args (rest (rest args)))
            (if (empty? args)
              (abort "malformed expression"))
            (set! alias (first args))))
        (set! specifiers (adjoin (list symbol alias) specifiers))
        (set! args (rest args))))
      
		`(spartan.core:%do-import ',package-name ',local-alias ',specifiers)))
