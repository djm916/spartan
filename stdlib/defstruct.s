
; Basic structures (a.k.a, records) library
;
; A structure is a compound data type composed of a set of named fields.
;
; Structures are defined with the "defstruct" macro, which results in the automatic definition
; of a constructor, field accessors, field mutators, and type predicate.
;
; For example, (defstruct point (x y)) defines the following procedures:
;
; (make-point x y)      ; returns a new point initialized with the given x and y values
; (point-x p)           ; returns the x value of point p
; (point-y p)           ; returns the y value of point p
; (point-set-x! p x)    ; sets the x value of point p
; (point-set-y! p y)    ; sets the y value of point p
; (point? o)            ; determines if o is a point
;
; Implementation notes:
;
; Structures are implemented as tables with the field names as keys mapping to the field values.
; A special key '__type maps to the structure name, in order to distinguish structure types.
; So, fields can be accessed and mutated as follows:
;
; (p 'x)                ; returns the x value of point p
; (set! (p 'x) new-x)   ; sets the x value of point p

(in-package 'spartan.core)

; Generate the name of a structure constructor
(defun generate-constructor-name (name)
  (string->symbol (string-concat "make-" (symbol->string name))))

; Generate the structure constructor
(defun generate-constructor (name fields)
  `(defun ,(generate-constructor-name name) ,fields
     (spartan.core:make-struct ',name (list ,@fields))))

; Generate the name of a structure type predicate
(defun generate-predicate-name (name)
  (string->symbol (string-concat (symbol->string name) "?")))

; Generate the structure type predicate
(defun generate-predicate (name)
  `(defun ,(generate-predicate-name name) (self)
     (spartan.core:= (spartan.core:type self) ',name)))

(defmacro defstruct (name fields)
  `(begin
     ; Register struct type
     (spartan.core:register-struct-type ',name ',fields)
     ; Define constructor
     ,(generate-constructor name fields)
     ; Define type predicate
     ,(generate-predicate name)))
