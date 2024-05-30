
; Basic records library
;
; A record is a compound data type composed of a set of named fields.
;
; Records are defined with the "defrecord" macro, which results in the automatic
; definition of a constructor, type predicate, and accessors and mutators for
; all fields.
;
; For example, (defstruct point (x y)) defines the following procedures:
;
; (make-point x y)      ; returns a new point initialized with the given x and y values
; (point-x p)           ; returns the x value of point p
; (point-y p)           ; returns the y value of point p
; (point-set-x! p x)    ; sets the x value of point p
; (point-set-y! p y)    ; sets the y value of point p
; (point? o)            ; determines if o is a point

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
  (string->symbol (string-concat (symbol->string name) "-set-" (symbol->string field) "!")))

(defmacro defrecord (name fields)
  `(do
     ; Register struct type
     (spartan.core:register-record-type ',name ',fields)
     ; Define constructor
     (def ,(generate-constructor-name name) (spartan.core:record-constructor ',name))
     ; Define type predicate
     (def ,(generate-predicate-name name) (spartan.core:record-predicate ',name))
     ; Define accessors
     ,@(spartan.core:list-map (fun (field) `(def ,(generate-accessor-name name field) (spartan.core:record-accessor ',name ',field))) fields)
     ; Define mutators
     ,@(spartan.core:list-map (fun (field) `(def ,(generate-mutator-name name field) (spartan.core:record-mutator ',name ',field))) fields)))

