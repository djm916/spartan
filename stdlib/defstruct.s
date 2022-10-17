
; Generate the function name for a structure constructor
;
; e.g., if the structure name is 'point, generate 'make-point

(defun generate-constructor-name (name)
  (string->symbol (string/concat "make-" (symbol->string name))))

; Generate the name of a structure accessor ("getter") function
;
; e.g., if the structure name is 'point, and the field is 'x,
; generate 'point-x

(defun generate-getter-name (name field)
  (string->symbol (string/concat (symbol->string name) "-" (symbol->string field))))

; Generate the name of a structure mutator ("setter") function
;
; e.g., if the structure name is 'point, and the field is 'x,
; generate 'set-point-x!

(defun generate-setter-name (name field)
  (string->symbol (string/concat "set-" (symbol->string name) "-" (symbol->string field) "!")))

; Generate the name of a structure predicate function to distinguish
; different structure types.
;
; e.g., if the structure name is 'point, generate 'point?

(defun generate-predicate-name (name)
  (string->symbol (string/concat (symbol->string name) "?")))

; Generate a structure constructor function
;
; e.g., if the structure name is 'point and the fields are '(x y),
; we generate the function
;
; (defun make-point (x y) ...)
;
; whichs takes the initial field values as arguments.
;
; When called, the constructor creates a unique structure instance
; whose fields are initialized to the given values.
;
; This internal representation uses a vector where the first element
; is a unique type-tag for the structure and the remaining elements
; store the field values.

(defun generate-constructor (struct-name fields)
  `(defun ,(generate-constructor-name struct-name)
          ,fields
          (vector/of ',struct-name ,@fields)))

(defun generate-getter (name)
  (fun (field index)
    `(defun ,(generate-getter-name name field)
            (self)
            (vector/ref self ,index))))

(defun generate-getters (name fields)
  (map-with-index (generate-getter name) 1 fields))

(defun generate-setter (name)
  (fun (field index)
    `(defun ,(generate-setter-name name field)
            (self value)
            (vector/set! self ,index value))))

(defun generate-setters (name fields)
  (map-with-index (generate-setter name) 1 fields))

(defun generate-predicate (name)
  `(defun ,(generate-predicate-name name) (self)
     (and (= (type self) 'type/vector)
          (> (length self) 0)
          (= (vector/ref self 0) ',name))))

(defmacro defstruct (name fields)  
  `(do
     ,(generate-constructor name fields)
     ,@(generate-getters name fields)
     ,@(generate-setters name fields)
     ,(generate-predicate name)))
