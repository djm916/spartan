
; defrecord - a simple syntactic record type facility
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
; For example, (defrecord point (x y)) defines the following:
;
; point          ; the record type descriptor
; make-point     ; constructor
; point?         ; predicate
; point-x        ; field accessors
; point-y
; set-point-x!   ; field mutators
; set-point-y!

(in-module spartan.base)

(export defrecord)

(defmacro defrecord (name fields)
  ; Generate the name of a record constructor
  (defun constructor-name (name)
    (string->symbol (string-concat "make-" (symbol->string name))))

  ; Generate the name of a record type predicate
  (defun predicate-name (name)
    (string->symbol (string-concat (symbol->string name) "?")))

  ; Generate the name of a record field accessor
  (defun accessor-name (name field)
    (string->symbol (string-concat (symbol->string name) "-" (symbol->string field))))

  ; Generate the name of a record field mutator
  (defun mutator-name (name field)
    (string->symbol (string-concat "set-" (symbol->string name) "-" (symbol->string field) "!")))

  `(do
     ; Bind record type name to the record type descriptor
     (def ,name (spartan.base:make-record-type ',name ',fields))
     ; Define constructor
     (def ,(constructor-name name) (spartan.base:record-constructor ,name))
     ; Define type predicate
     (def ,(predicate-name name) (spartan.base:record-predicate ,name))
     ; Define accessors
     ,@(spartan.base:map (fun (field) `(def ,(accessor-name name field) (spartan.base:record-accessor ,name ',field))) fields)
     ; Define mutators
     ,@(spartan.base:map (fun (field) `(def ,(mutator-name name field) (spartan.base:record-mutator ,name ',field))) fields)))
