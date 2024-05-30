(in-package spartan.hashmap)

(defrecord map-type (hash-fn equal-fn table size))

(defun make-hashmap (hash-fn equal-fn)
  (def initial-capacity 2)
  (def table (make-vector initial-capacity ()))
  (make-map-type hash-fn equal-fn table 0))

(defun hashmap? (self)
  (map-type? self))

(defun empty? (self)
  (= 0 (map-type-size self)))

(defun size (self)
  (map-type-size self))

(defun insert! (self key value)
  (def table (map-type-table self))
  (def capacity (vector-length table))
  (def size (map-type-size self))
  (def hash (map-type-hash-fn self))
  (def equal? (map-type-equal-fn self))
  (def index (remainder (hash key) capacity))
  (def node (__find-node table equal? index key))
  (if (null? node)
    ; Key doesn't exist, add new (key, value) pair to bucket
    (let ((node-list (vector-ref table index))
          (new-node (list key value)))
      (vector-set! table index (cons new-node node-list))
      (map-type-set-size! self (+ 1 size))
      ; Expand table capacity when load factor exceeded
      (if (> (/ size capacity) 0.75)
        (__resize-to-capacity! self)))
    ; Key exists, replace current value associated with key
    (set-car! (cdr node) value)))

(defun contains? (self key)
  (def table (map-type-table self))
  (def capacity (vector-length table))
  (def size (map-type-size self))
  (def hash (map-type-hash-fn self))
  (def equal? (map-type-equal-fn self))
  (def index (remainder (hash key) capacity))
  (def node (__find-node table equal? index key))
  (not (null? node)))

(defun remove! (self key)
  (def table (map-type-table self))
  (def capacity (vector-length table))
  (def size (map-type-size self))
  (def hash (map-type-hash-fn self))
  (def equal? (map-type-equal-fn self))
  (def index (remainder (hash key) capacity))
  (def node-list (vector-ref table index))
  (def node-and-prev (__find-node-for-removal table equal? index key))
  (if (not (null? node-and-prev))
    (let ((node (car node-and-prev))
          (prev (cadr node-and-prev)))
      (if (void? prev)
        (vector-set! table index ())
        (set-cdr! prev (cdr node)))
      (map-type-set-size! self (- size 1)))))

(defun find-or-default (self key default)
  (def table (map-type-table self))
  (def capacity (vector-length table))
  (def size (map-type-size self))
  (def hash (map-type-hash-fn self))
  (def equal? (map-type-equal-fn self))
  (def index (remainder (hash key) capacity))
  (def node (__find-node table equal? index key))
  (if (null? node) default (cadr node)))

(defun find (self key)
  (find-or-default self key void))

(defun for-each (self proc)
  (def table (map-type-table self))
  (def capacity (vector-length table))
  (def index 0)
  (while (< index capacity)
    (let ((node-list (vector-ref table index)))
      (while (not (null? node-list))
        (let ((node (car node-list)))
          (proc (car node) (cadr node))
          (set! node-list (cdr node-list)))))
    (inc! index)))

(defun entries (self)
  (def result ())
  (for-each self 
    (fun (key value)
      (set! result (cons (list key value) result))))
  result)

(defun keys (self)
  (list-map (fun (pair) (car pair)) (entries self)))

(defun values (self)
  (list-map (fun (pair) (cadr pair)) (entries self)))

(defun __find-node (table equal? index key)
  (def node-list (vector-ref table index))
  (def found false)
  (while (and (not found) (not (null? node-list)))
    (let ((node (car node-list)))
      (if (equal? key (car node))
        (set! found true)
        (set! node-list (cdr node-list)))))
  (if found (car node-list) ()))

(defun __find-node-for-removal (table equal? index key)
  (def prev void)
  (def node-list (vector-ref table index))
  (def found false)
  (while (and (not found) (not (null? node-list)))
    (let ((node (car node-list)))
      (if (equal? key (car node))
        (set! found true)
        (do
          (set! node-list (cdr node-list))
          (set! prev node)))))
  (if found (list (car node-list) prev) ()))

(defun __resize-to-capacity! (self)
  (def old-table (map-type-table self))
  (def old-capacity (vector-length old-table))
  (def new-capacity (* 2 old-capacity))
  (def new-table (make-vector new-capacity ()))
  (map-type-set-table! self new-table)
  (map-type-set-size! self 0)
  (let ((index 0))
    (while (< index old-capacity)
      (let ((node-list (vector-ref old-table index)))
        (while (not (null? node-list))
          (let ((node (car node-list)))
            (insert! self (car node) (cadr node)))
          (set! node-list (cdr node-list))))
      (inc! index))))
