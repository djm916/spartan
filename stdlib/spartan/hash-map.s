(in-ns spartan.hash-map.internal)

(defrecord hash-map (hash-fn equal-fn table size))

(defun find-node (table equal? index key)
  (def node-list (vector-ref table index))
  (def found #false)
  (while (and (not found) (not (empty? node-list)))
    (let ((node (first node-list)))
      (if (equal? key (first node))
        (set! found #true)
        (set! node-list (rest node-list)))))
  (if found (first node-list) ()))

(defun find-node-for-removal (table equal? index key)
  (def prev #nil)
  (def node-list (vector-ref table index))
  (def found #false)
  (while (and (not found) (not (empty? node-list)))
    (let ((node (first node-list)))
      (if (equal? key (first node))
        (set! found #true)
        (do
          (set! node-list (rest node-list))
          (set! prev node)))))
  (if found (list (first node-list) prev) ()))

(defun resize-to-capacity! (self)
  (def old-table (hash-map-table self))
  (def old-capacity (vector-length old-table))
  (def new-capacity (* 2 old-capacity))
  (def new-table (make-vector new-capacity ()))
  (set-hash-map-table! self new-table)
  (set-hash-map-size! self 0)
  (let ((index 0))
    (while (< index old-capacity)
      (let ((node-list (vector-ref old-table index)))
        (while (not (empty? node-list))
          (let ((node (first node-list)))
            (spartan.hash-map:insert! self (first node) (second node)))
          (set! node-list (rest node-list))))
      (inc! index))))

(in-ns spartan.hash-map)
(import spartan.hash-map.internal :as internal)

(defun make-hash-map (hash-fn equal-fn)
  (def initial-capacity 2)
  (def table (make-vector initial-capacity ()))
  (internal:make-hash-map hash-fn equal-fn table 0))

(defun hash-map? (self)
  (internal:hash-map? self))

(defun empty? (self)
  (= 0 (internal:hash-map-size self)))

(defun size (self)
  (internal:hash-map-size self))

(defun insert! (self key value)
  (match self
    ((record internal:hash-map hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node (internal:find-node table equal-fn index key)))
       (if (not (spartan.core:empty? node))
         ; Key exists, replace current value associated with key
         (set-first! (rest node) value)
         ; Key doesn't exist, add new (key, value) pair to bucket
         (let ((node-list (vector-ref table index))
               (new-node (list key value)))
           (vector-set! table index (adjoin new-node node-list))
           (internal:set-hash-map-size! self (+ 1 size))
           ; Expand table capacity when load factor exceeded
           (if (> (/ size capacity) 0.75)
             (internal:resize-to-capacity! self))))))))

(defun contains? (self key)
  (match self
    ((record internal:hash-map hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node (internal:find-node table equal-fn index key)))
       (not (spartan.core:empty? node))))))

(defun remove! (self key)
  (match self
    ((record internal:hash-map hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node-list (vector-ref table index))
            (node-and-prev (internal:find-node-for-removal table equal-fn index key)))
       (if (not (spartan.core:empty? node-and-prev))
         (let ((node (first node-and-prev))
               (prev (second node-and-prev)))
           (if (nil? prev)
             (vector-set! table index ())
             (set-rest! prev (rest node)))
           (internal:set-hash-map-size! self (- size 1))))))))

(defun find-or-default (self key default)
  (match self
    ((record internal:hash-map hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node (internal:find-node table equal-fn index key)))
       (if (spartan.core:empty? node) default (second node))))))

(defun find (self key)
  (find-or-default self key #nil))

(defun for-each (self proc)
  (match self
    ((record internal:hash-map hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index 0))
       (while (< index capacity)
         (let ((node-list (vector-ref table index)))
           (while (not (spartan.core:empty? node-list))
             (let ((node (first node-list)))
               (proc (first node) (second node))
               (set! node-list (rest node-list)))))
         (inc! index))))))

(defun entries (self)
  (def result ())
  (for-each self 
    (fun (key value)
      (set! result (adjoin (list key value) result))))
  result)

(defun keys (self)
  (map (fun (pair) (first pair)) (entries self)))

(defun values (self)
  (map (fun (pair) (second pair)) (entries self)))
