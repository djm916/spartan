
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

; Generate the table key for a field

;(defun generate-field-key (field)
;  (string->symbol (string.concat ":" (symbol->string field))))

;(defun generate-table-entries (fields)
;  (rec loop [(fields fields) (keys (map generate-field-key fields))]
;    (if (empty? fields) ()
;      (cons (car keys) (cons (car fields) (loop (cdr fields) (cdr keys)))))))

(in-package 'spartan.core)

(defun generate-table-entries (fields dummy-params)
  (if (empty? fields) ()
    (let [(field (car fields)) (dummy-param (car dummy-params))]
      `(',field ,dummy-param ,@(generate-table-entries (cdr fields) (cdr dummy-params))))))

; Generate the name of a structure type predicate

(defun generate-predicate-name (name)
  (string->symbol (string-concat (symbol->string name) "?")))

(defun generate-dummy-params (num-fields)
  (if (= 0 num-fields) ()
    (let [(dummy-param (gensym))]
      (cons dummy-param (generate-dummy-params (- num-fields 1))))))

(defun generate-constructor-name (name)
  (string->symbol (string-concat "make-" (symbol->string name))))

; Generate the structure constructor

(defun generate-constructor (name fields)
  (let [(dummy-params (generate-dummy-params (length fields)))]
    `(defun ,(generate-constructor-name name) ,dummy-params
       (table '__type ',name ,@(generate-table-entries fields dummy-params)))))

; Generate the structure type predicate

(defun generate-predicate (name)
  (let* [(dummy-params (generate-dummy-params 1))
         (self (car dummy-params))]
    `(defun ,(generate-predicate-name name) ,dummy-params
       (and (table? ,self)
            (= (,self '__type) ',name)))))

(defmacro defstruct (name fields)
  `(do
     ,(generate-constructor name fields)
     ,(generate-predicate name)))
