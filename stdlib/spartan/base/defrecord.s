
; defrecord - a simple facility for record definitions
;
; A record is a compound data type composed of a set of named fields.
; Each record type definition results in the definition of:
;
;   * A record type descriptor that represents the record type itself
;   * A constructor: accepts values for each field and returns a new instance of this record type
;   * A type predicate: tests if an object is an instance of this record type
;   * An accessor for each field: returns the field value
;   * A mutator for each mutable field: updates the field value
;
; Records are defined with the "defrecord" macro, which has the syntax:
;
; <record type definition> =>
;   (defrecord <type name>
;	    <constructor name>
;     <predicate name>
;     <field> ...)
;
; <field> => (<field name> <accessor name>)
;         => (<field name> <accessor name> <mutator name>)
;
; For example, the record definition
;
; (defrecord Point
;   point
;   point?
;   (x point-x point-set-x!)
;   (y point-y point-set-y!))
;
; results in the following definitions:
;
; Point          ; the record type descriptor
; point          ; constructor
; point?         ; predicate
; point-x        ; field accessors
; point-y
; point-set-x!   ; field mutators
; point-set-y!

(in-module spartan.base)

(export defrecord)

(defmacro defrecord (name cons pred :rest fields)
  `(do
     ; Bind record type name to the record type descriptor
     (def ,name (spartan.base:make-record-type ',name ',(spartan.base:map spartan.base:first fields)))
     ; Define constructor
     (def ,cons (spartan.base:record-constructor ,name))
     ; Define type predicate
     (def ,pred (spartan.base:record-predicate ,name))
     ; Define accessors
     ,@(spartan.base:map (fun (f) `(def ,(second f) (spartan.base:record-accessor ,name ',(first f)))) fields)
     ; Define mutators
     ,@(spartan.base:map (fun (f) `(def ,(third f) (spartan.base:record-mutator ,name ',(first f))))
                         (spartan.base:filter (fun (f) (= 3 (length f))) fields))))
