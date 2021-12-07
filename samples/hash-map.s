
(load "stdlib/defstruct.s")

(defstruct hash-map (hash-fun
                     eq-fun
                     table
                     size))

(defun hash-map/new (hash-fun eq-fun)
  (make-hash-map hash-fun eq-fun (vector/new 4 ()) 0))

(defun hash-map/empty? (self)
  (= 0 (hash-map-size self)))

(defun hash-map/insert (self key value)
  (def index (% ((hash-map-hash-fun self) key)
                (length (hash-map-table self))))
  (def node (find-node-with-key self index key))
  (cond ((empty? node)
         (vector/set! (hash-map-table self) index
           (cons (list key value)
                 (vector/get (hash-map-table self) index)))
         (set-hash-map-size! self (+ 1 (hash-map-size self)))
         (if (> (/ (int->real (hash-map-size self))
                   (int->real (length (hash-map-table self))))
                0.75)
           (do (resize-to-capacity self)
               (hash-map/insert self key value))))
        ; Replace existing value associated with key
        (true
         (set-car! (cdr node) value))))

(defun hash-map/remove (self key)
  (def table (hash-map-table self))
  (def capacity (length table))
  (def index (% ((hash-map-hash-fun self) key) capacity))
  (def node-list (vector/get table index))
  (defun node-has-key? (node) ((hash-map-eq-fun self) key (car node)))
  (vector/set! table index (remove! node-has-key? node-list)))
  
(defun hash-map/find (self key)
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

(defun resize-to-capacity (self)
  (def old-table (hash-map-table self))
  (def old-capacity (length old-table))
  (def new-capacity (* 2 old-capacity))
  (set-hash-map-table! self (vector/new new-capacity ()))
  (set-hash-map-size! self 0)
  (let ((index 0))
    (while (< index old-capacity)
      (let ((node-list (vector/get old-table index)))
        (while (not (empty? node-list))
          (let ((node (car node-list)))
            (hash-map/insert self (car node) (cadr node)))
          (set! node-list (cdr node-list))))
      (set! index (+ 1 index)))))
  
(defun hash-int (n) n)

(def h (hash-map/new hash-int =))
(hash-map/insert h 1 "a")
(hash-map/find h 1)
(hash-map/insert h 2 "b")
(hash-map/find h 2)
(hash-map/insert h 3 "c")
(hash-map/find h 3)
(hash-map/insert h 4 "d")
(hash-map/find h 4)
