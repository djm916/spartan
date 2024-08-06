(in-package spartan.core)

; 

(defun %do-import (source-package import-specifiers)
  (for ((specifiers import-specifiers (cdr specifiers)))
    ((null? specifiers) void)
    (let* ((spec (car specifiers))
           (symbol (if (list? spec) (car spec) spec))
           (alias (if (list? spec) (cadr spec) symbol)))
      (package-bind alias (package-resolve symbol source-package)))))
      
;  (rec loop ((elems import-specifiers))
;    (if (empty? elems) ()
;      (let* ((elem (car elems))
;             (symbol (if (list? elem) (car elem) elem))
;             (alias (if (list? elem) (cadr elem) symbol)))
;        (package-bind *package* alias (package-resolve source-package symbol))
;        (loop (cdr elems))))))

; <import-statement> => (import <package> (:as <alias>)? <import-specifier>*)
; <import-specifier> => <symbol> (:as <alias>)?
; <package> => <symbol>
; <alias> => <symbol>

(defmacro import (package-name & args)
  
  (let ((imported-symbols ())
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
        (set! imported-symbols (cons (list symbol alias) imported-symbols))
        (set! args (cdr args))))
    
    `(let* ((source-package (find-package ',package-name))
            (import-specifiers ,(if (null? imported-symbols)
                                  `(package-symbols source-package)
                                  `',imported-symbols)))
       ,@(if (void? local-alias) () `((package-alias ',package-name ',local-alias)))
       (%do-import source-package import-specifiers))))
