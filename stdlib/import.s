(in-package spartan.core)

; 

(defun %do-import (package-name local-alias specifiers)
  (if (not (void? local-alias))
	  (package-alias package-name local-alias))
	(let* ((from-pkg (the-package package-name))
	       (specifiers (if (null? specifiers)
				               (list-map (fun (s) (list s s)) (package-symbols from-pkg))
											 specifiers)))
		(for ((spec specifiers (cdr spec)))
			((null? spec) void)
			(let-values (((symbol alias) (car spec)))
				(package-bind alias (package-resolve symbol from-pkg))))))

; <import-statement> => (import <package> (:as <alias>)? <import-specifier>*)
; <import-specifier> => <symbol> (:as <alias>)?
; <package> => <symbol>
; <alias> => <symbol>

(defmacro import (package-name & args)
  (let ((specifiers ())
        (local-alias void))    
    ; parse optional local package alias
    (if (and (not (null? args)) (= (car args) :as))
      (do
        (set! args (cdr args))
        (if (null? args)
          (abort "malformed expression"))
        (set! local-alias (car args))
        (set! args (cdr args))))
    
    ; parse imported symbols with optional aliases
    (while (not (null? args))
      (let* ((symbol (car args))
             (alias symbol))
        (if (and (not (null? (cdr args))) (= (cadr args) :as))
          (do
            (set! args (cdr args))
            (set! args (cdr args))
            (if (null? args)
              (abort "malformed expression"))
            (set! alias (car args))))
        (set! specifiers (cons (list symbol alias) specifiers))
        (set! args (cdr args))))
      
		`(spartan.core:%do-import ',package-name ',local-alias ',specifiers)))
