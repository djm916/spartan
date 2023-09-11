
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

(in-package 'spartan.core)

; Generate the name of a structure type predicate

(defun generate-predicate-name (name)
  (string->symbol (string-concat (symbol->string name) "?")))

; Generate the name of a structure constructor

(defun generate-constructor-name (name)
  (string->symbol (string-concat "make-" (symbol->string name))))

; Generate the table entries for the underlying structure representation

(defun generate-table-entries (fields)
  (if (empty? fields) ()
    `(',(car fields) ,(car fields) ,@(generate-table-entries (cdr fields)))))

; Generate the structure constructor

(defun generate-constructor (name fields)
  `(defun ,(generate-constructor-name name) ,fields
     (spartan.core:table '__type ',name ,@(generate-table-entries fields))))

; Generate the structure type predicate

(defun generate-predicate (name)
  `(defun ,(generate-predicate-name name) (self)
     (and (spartan.core:table? self)
          (spartan.core:= (self '__type) ',name))))

(defmacro defstruct (name fields)
  `(do
     ,(generate-constructor name fields)
     ,(generate-predicate name)))
