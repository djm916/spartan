(in-package spartan.core)

; 

(defun %import (source-package import-specifiers)
  (rec loop ((elems import-specifiers))
    (if (empty? elems) ()
      (let* ((elem (car elems))
             (symbol (if (list? elem) (car elem) elem))
             (alias (if (list? elem) (cadr elem) symbol)))
        (package-bind *package* alias (package-resolve source-package symbol))
        (loop (cdr elems))))))

; <import-statement> => (import <package> (:as <alias>)? <import-specifier>*)
; <import-specifier> => <symbol> (:as <alias>)?
; <package> => <symbol>
; <alias> => <symbol>

(defmacro import (package-name & args)
  
  (let ((imported-symbols ())
        (local-alias nil))
    
    ; parse optional local package alias
    (if (and (not (empty? args)) (= (car args) :as))
      (begin
        (set! args (cdr args))
        (if (empty? args)
          (error "malformed expression"))
        (set! local-alias (car args))
        (set! args (cdr args))))
    
    ; parse imported symbols with optional aliases
    (while (not (empty? args))
      (let* ((symbol (car args))
             (alias symbol))
        (if (and (not (empty? (cdr args))) (= (cadr args) :as))
          (begin
            (set! args (cdr args))
            (set! args (cdr args))
            (if (empty? args)
              (error "malformed expression"))
            (set! alias (car args))))
        (set! imported-symbols (cons (list symbol alias) imported-symbols))
        (set! args (cdr args))))
    
    `(let* ((source-package (find-package ',package-name))
            (import-specifiers ,(if (empty? imported-symbols)
                                  `(package-symbols source-package)
                                  `',imported-symbols)))
       ,@(if (nil? local-alias) () `((package-add-alias *package* source-package ',local-alias)))
       (%import source-package import-specifiers))))
       