
; Basic syntactic records library
;
; A record is a compound data type composed of a set of named fields.
;
; Records are defined with the "defrecord" macro, which results in the
; implicit definitions of the following:
; 
;   * a record type descriptor
;   * a positional constructor
;   * a type predicate
;   * accessors and mutators for each field
;
; For example, (defstruct point (x y)) defines the following:
;
; point-type     ; the record type descriptor
; make-point     ; the constructor
; point?         ; predicate
; point-x        ; accessors
; point-y
; point-x-set!   ; mutators
; point-y-set!

(in-package spartan.core)

; Generate the name of a record constructor
(defun generate-constructor-name (name)
  (string->symbol (string-concat "make-" (symbol->string name))))

; Generate the name of a record type predicate
(defun generate-predicate-name (name)
  (string->symbol (string-concat (symbol->string name) "?")))

; Generate the name of a record field accessor
(defun generate-accessor-name (name field)
  (string->symbol (string-concat (symbol->string name) "-" (symbol->string field))))

; Generate the name of a record field mutator
(defun generate-mutator-name (name field)
  (string->symbol (string-concat (symbol->string name) "-" (symbol->string field) "-set!")))

; Generate the name of a record destructor
(defun generate-destructor-name (name)
  (string->symbol (string-concat "unpack-" (symbol->string name))))
  
; Generate the name of a record matcher
(defun generate-matcher-name (name)
  (string->symbol (string-concat "with-" (symbol->string name))))

(defmacro defrecord (name fields)
  `(let ((rtd (spartan.core:make-record-type ',name ',fields))) ; Create record type descriptor
     ; Bind record type name to the record type descriptor
     ;(def ,name rtd)
     ; Define constructor
     (def ,(generate-constructor-name name) (spartan.core:record-constructor rtd))
     ; Define type predicate
     (def ,(generate-predicate-name name) (spartan.core:record-predicate rtd))
     ; Define accessors
     ,@(spartan.core:list-map (fun (field) `(def ,(generate-accessor-name name field) (spartan.core:record-accessor rtd ',field))) fields)
     ; Define mutators
     ,@(spartan.core:list-map (fun (field) `(def ,(generate-mutator-name name field) (spartan.core:record-mutator rtd ',field))) fields)
     ; Define destructor
     (def ,(generate-destructor-name name) (spartan.core:record-destructor rtd))
     ; Define matcher
     (defmacro ,(generate-matcher-name name) (arg vars & body)
       `(apply (fun ,vars ,@body) (',,(generate-destructor-name name) ,arg)))))
