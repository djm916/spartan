(require "stdlib/defstruct.s")

(defstruct __hash-map-impl (hash equal? table size))

(defun hash-map (hash equal?)
  (def initial-capacity 8)
  (def table (vector/fill initial-capacity ()))
  (__hash-map-impl hash equal? table 0))

(def hash-map? __hash-map-impl?)

(defun hash-map/empty? (self) (= 0 (hash-map/size self)))

(def hash-map/size __hash-map-impl/size)

(defun hash-map/insert (self key value)
  (def table (__hash-map-impl/table self))
  (def capacity (length table))
  (def size (__hash-map-impl/size self))
  (def hash (__hash-map-impl/hash self))
  (def equal? (__hash-map-impl/equal? self))
  (def index (remainder (hash key) capacity))
  (def node (__find-node-with-key table equal? index key))
  (if (empty? node)
    ; Key doesn't exist, add new (key, value) pair to bucket
    (let ((node-list (table index))
          (new-node (list key value)))
      (vector/set! table index (cons new-node node-list))
      (__hash-map-impl/set-size! self (+ 1 size))
      ; Expand table capacity when load factor exceeded
      (if (> (/ size capacity) 0.75)
        (__resize-to-capacity! self)))
    ; Key exists, replace current value associated with key
    (set-car! (cdr node) value)))

(defun hash-map/key-exists? (self key)
  (def table (__hash-map-impl/table self))
  (def capacity (length table))
  (def hash (__hash-map-impl/hash self))
  (def equal? (__hash-map-impl/equal? self))
  (def index (remainder (hash key) capacity))
  (def node (__find-node-with-key table equal? index key))
  (not (empty? node)))

(defun hash-map/remove (self key)
  (def table (__hash-map-impl/table self))
  (def capacity (length table))
  (def hash (__hash-map-impl/hash self))
  (def equal? (__hash-map-impl/equal? self))
  (def index (remainder (hash key) capacity))
  (def node-list (table index))
  (defun node-has-key? (node) (equal? key (car node)))
  (vector/set! table index (remove! node-has-key? node-list)))
  
(defun hash-map/find (self key)
  (def table (__hash-map-impl/table self))
  (def capacity (length table))
  (def hash (__hash-map-impl/hash self))
  (def equal? (__hash-map-impl/equal? self))
  (def index (remainder (hash key) capacity))
  (def node (__find-node-with-key table equal? index key))
  (if (empty? node) nil (cadr node)))

(defun hash-map/for-each (self proc)
  (def table (__hash-map-impl/table self))
  (def capacity (length table))
  (def index 0)
  (while (< index capacity)
    (let ((node-list (table index)))
      (while (not (empty? node-list))
        (let ((node (car node-list)))
          (proc (car node) (cadr node))
          (set! node-list (cdr node-list)))))
    (set! index (+ 1 index))))

(defun hash-map/items (self)
  (def item-list ())
  (hash-map/for-each self 
    (fun (key value)
      (set! item-list (cons (list key value) item-list))))
  item-list)

(defun hash-map/keys (self)
  (map (fun (pair) (car pair)) (hash-map/items self)))

(defun hash-map/values (self)
  (map (fun (pair) (cadr pair)) (hash-map/items self)))

(defun __find-node-with-key (table equal? index key)
  (def node-list (table index))
  (def found false)
  (while (and (not found) (not (empty? node-list)))
    (if (equal? key (caar node-list))
      (set! found true)
      (set! node-list (cdr node-list))))
  (if found (car node-list) ()))

(defun __resize-to-capacity! (self)
  (def old-table (__hash-map-impl/table self))
  (def old-capacity (length old-table))
  (def new-capacity (* 2 old-capacity))
  (def new-table (vector/fill new-capacity ()))
  (__hash-map-impl/set-table! self new-table)
  (__hash-map-impl/set-size! self 0)
  (let ((index 0))
    (while (< index old-capacity)
      (let ((node-list (old-table index)))
        (while (not (empty? node-list))
          (let ((node (car node-list)))
            (hash-map/insert self (car node) (cadr node)))
          (set! node-list (cdr node-list))))
      (set! index (+ 1 index)))))

; Test example

(defun identity (x) x)

(def h (hash-map identity =))

(hash-map/insert h 1 "a")
(hash-map/insert h 2 "b")
(hash-map/insert h 3 "c")
(hash-map/insert h 4 "d")
(hash-map/insert h 5 "e")

(print-line (hash-map/items h))
(print-line (hash-map/keys h))
(print-line (hash-map/values h))
(print-line (hash-map/size h))