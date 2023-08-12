
; Basic structures (a.k.a, records) library
;
; A structure is a compound data type composed of a set of named fields.
;
; Structures are defined with the "defstruct" macro, which results in the automatic definition
; of constructor, field accessors, field mutators, and type predicate functions.
;
; For example, (defstruct point x y) defines the following procedures:
;
; (point x y)           ; returns a new point initialized with the given x and y values
; (point/x p)           ; returns the x value of point p
; (point/y p)           ; returns the y value of point p
; (point/set-x! p x)    ; sets the x value of point p
; (point/set-y! p y)    ; sets the y value of point p
; (point? o)            ; determines if o is a point
;
; Implementation notes:
;
; Structures are implemented as vectors where the first element is the name of the structure,
; (e.g., the symbol 'point), and whose remaining elements store the field values.

; Generate the name of a structure accessor (getter)

(defun generate-accessor-name (name field-name)
  (string->symbol (string:concat (symbol->string name) ":" (symbol->string field-name))))

; Generate the name of a structure mutator ("setter")

(defun generate-mutator-name (name field-name)
  (string->symbol (string:concat (symbol->string name) ":set-" (symbol->string field-name) "!")))

; Generate the name of a structure type predicate

(defun generate-predicate-name (name)
  (string->symbol (string:concat (symbol->string name) "?")))

; Generate a structure constructor

(defun generate-constructor (name fields)
  `(defun ,name ,fields
     (vector ',name ,@fields)))

(defun generate-accessor (name)
  (fun (field index)
    `(defun ,(generate-accessor-name name field) (self)
       (vector:ref self ,index))))

(defun generate-accessors (name fields)
  (map/index (generate-accessor name) 1 fields))

(defun generate-mutator (name)
  (fun (field index)
    `(defun ,(generate-mutator-name name field) (self value)
       (vector:set! self ,index value))))

(defun generate-mutators (name fields)
  (map/index (generate-mutator name) 1 fields))

(defun generate-predicate (name)
  `(defun ,(generate-predicate-name name) (self)
     (and (vector? self)
          (> (vector:length self) 0)
          (= (vector:ref self 0) ',name))))

(defmacro defstruct (name fields)
  `(do
     ,(generate-constructor name fields)
     ,@(generate-accessors name fields)
     ,@(generate-mutators name fields)
     ,(generate-predicate name)))
