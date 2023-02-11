
; Basic structures (a.k.a, records) library
;
; A structure is a named, compound data value composed of 0 or more
; named fields.
;
; Structures are defined with the "defstruct" macro. Given the name
; and fields of a structure, this results in the automatic definition
; of a constructor, field accessors, field mutators, and a type predicate.
;
; For example, (defstruct point x y) defines the following procedures:
;
; (point init-x init-y)
; (point/x point)
; (point/y point)
; (point/set-x! point new-x)
; (point/set-y! point new-y)
; (point? any-value)
;
; The internal representation is a vector where the first element
; is a unique type-tag (symbol) for the structure and the remaining
; elements store the field values.

; Generate the name of a structure accessor ("getter") function
;
; e.g., if the structure name is point, and the field is x,
; generate point/x

(defun generate-accessor-name (name field-name)
  (string->symbol (string/concat (symbol->string name) "/" (symbol->string field-name))))

; Generate the name of a structure mutator ("setter") function
;
; e.g., if the structure name is point, and the field is x,
; generate point/set-x!

(defun generate-mutator-name (name field-name)
  (string->symbol (string/concat (symbol->string name) "/set-" (symbol->string field-name) "!")))

; Generate the name of a structure type predicate function to distinguish
; different structure types.
;
; e.g., if the structure name is point, generate point?

(defun generate-predicate-name (name)
  (string->symbol (string/concat (symbol->string name) "?")))

; Generate a structure constructor function
;
; e.g., if the structure name is point and the fields are (x y),
; we generate the function
;
; (defun point (x y) ...)

(defun generate-constructor (name fields)
  `(defun ,name ,fields
     (vector ',name ,@fields)))

(defun generate-accessor (name)
  (fun (field index)
    `(defun ,(generate-accessor-name name field) (self)
       (at self ,index))))

(defun generate-accessors (name fields)
  (map-with-index (generate-accessor name) 1 fields))

(defun generate-mutator (name)
  (fun (field index)
    `(defun ,(generate-mutator-name name field) (self value)
       (set-at! self ,index value))))

(defun generate-mutators (name fields)
  (map-with-index (generate-mutator name) 1 fields))

(defun generate-predicate (name)
  `(defun ,(generate-predicate-name name) (self)
     (and (vector? self)
          (not (empty? self))
          (= (at self 0) ',name))))

(defmacro defstruct (name fields)
  `(do
     ,(generate-constructor name fields)
     ,@(generate-accessors name fields)
     ,@(generate-mutators name fields)
     ,(generate-predicate name)))
