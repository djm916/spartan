
(load "stdlib/defstruct.s")

(defstruct hash-map (hash-fun equal-fun table size))

(defun hash-map/new (hash-fun equal-fun)
  (def initial-capacity 4)
  (def table (vector/new initial-capacity ()))
  (make-hash-map hash-fun equal-fun table 0))

(defun hash-map/empty? (self) (= 0 (hash-map-size self)))

(defun hash-map/size (self) (hash-map-size self))

(defun hash-map/insert (self key value)
  (def table (hash-map-table self))
  (def size (hash-map-size self))
  (def capacity (length table))
  (def index (% ((hash-map-hash-fun self) key) capacity))
  (def node (find-node-with-key self index key))
  (if (empty? node)
    ; No key exists, add a new key, value pair
    (let ((node-list (vector/get table index))
          (new-node (list key value)))
      (vector/set! table index (cons new-node node-list))
      (set-hash-map-size! self (+ 1 size))
      ; Automatically expand table when load factor exceeded
      (if (> (/ (int->real size) (int->real capacity)) 0.75)
        (resize-to-capacity! self)))
    ; Key exists, replace current value
    (set-car! (cdr node) value)))

(defun hash-map/key-exists? (self key)
  (def table (hash-map-table self))
  (def capacity (length table))
  (def index (% ((hash-map-hash-fun self) key) capacity))
  (def node (find-node-with-key self index key))
  (not (empty? node)))

(defun hash-map/remove (self key)
  (def table (hash-map-table self))
  (def capacity (length table))
  (def index (% ((hash-map-hash-fun self) key) capacity))
  (def node-list (vector/get table index))
  (defun node-has-key? (node) ((hash-map-equal-fun self) key (car node)))
  (vector/set! table index (remove! node-has-key? node-list)))
  
(defun hash-map/find (self key)
  (def index (% ((hash-map-hash-fun self) key)
                (length (hash-map-table self))))
  (def node (find-node-with-key self index key))
  (if (empty? node) nil (cadr node)))

(defun hash-map/for-each (self proc)
  (def table (hash-map-table self))
  (def capacity (length table))
  (def index 0)
  (while (< index capacity)
    (let ((node-list (vector/get table index)))
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
  (map (fun (key value) key) (hash-map/items self)))

(defun hash-map/values (self)
  (map (fun (key value) value) (hash-map/items self)))

; Private API

(defun find-node-with-key (self index key)
  (def node-list (vector/get (hash-map-table self) index))
  (def found false)
  (while (and (not found) (not (empty? node-list)))
    (if ((hash-map-equal-fun self) (caar node-list) key)
      (set! found true)
      (set! node-list (cdr node-list))))
  (if found (car node-list) ()))

(defun resize-to-capacity! (self)
  (def old-table (hash-map-table self))
  (def old-capacity (length old-table))
  (def new-capacity (* 2 old-capacity))
  (def new-table (vector/new new-capacity ()))
  (set-hash-map-table! self new-table)
  (set-hash-map-size! self 0)
  (let ((index 0))
    (while (< index old-capacity)
      (let ((node-list (vector/get old-table index)))
        (while (not (empty? node-list))
          (let ((node (car node-list)))
            (hash-map/insert self (car node) (cadr node)))
          (set! node-list (cdr node-list))))
      (set! index (+ 1 index)))))

; Test example

(defun identity (x) x)
(def hash-int identity)

(def h (hash-map/new hash-int =))
(hash-map/insert h 1 "a")
(hash-map/find h 1)
(hash-map/insert h 2 "b")
(hash-map/find h 2)
(hash-map/insert h 3 "c")
(hash-map/find h 3)
(hash-map/insert h 4 "d")
(hash-map/find h 4)
(hash-map/insert h 5 "e")
(hash-map/find h 5)
(hash-map/for-each h 
  (fun (key value)
    (print-line key)
    (print-line value)))
