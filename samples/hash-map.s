
(load "stdlib/defstruct.s")

(defstruct hash-map (hash-fun
                     eq-fun
                     table
                     size))

(defun hash-map/new (hash-fun eq-fun)
  (make-hash-map hash-fun eq-fun (vector/new 16 ()) 0))

(defun hash-map/empty? (self)
  (= 0 (hash-map-size self)))

(defun hash-map/add (self key value)
  (def index (% ((hash-map-hash-fun self) key)
                (length (hash-map-table self))))
  (def node (find-node-with-key self index key))
  (cond ((empty? node)
         (vector/set! (hash-map-table self) index
           (cons (list key value)
                 (vector/get (hash-map-table self) index)))
         (set-hash-map-size! self (+ 1 (hash-map-size self))))
        (true
         (set-car! (cdr node) value))))

(defun hash-map/lookup (self key)
  (def index (% ((hash-map-hash-fun self) key)
                (length (hash-map-table self))))
  (def node (find-node-with-key self index key))
  (if (empty? node) nil (cadr node)))

(defun find-node-with-key (self index key)
  (def node-list (vector/get (hash-map-table self) index))
  (def found false)
  (while (and (not found) (not (empty? node-list)))
    (if ((hash-map-eq-fun self) (car (car node-list)) key)
      (set! found true)
      (set! node-list (cdr node-list))))
  (if found (car node-list) ()))

(defun int-hash (n) n)

(def h (hash-map/new int-hash =))
(hash-map/add h 18 "a")
(hash-map/add h 2 "b")
(hash-map/add h 18 "c")
