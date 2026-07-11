(in-module spartan.hash-map)

(export make-mapping
        mapping?
        empty?
        size
        contains?
        insert!
        remove!
        find
        find/default
        for-each
        entries
        keys
        values)

(defrecord hashtable (hash-fn equal-fn table size))

(defun find-node (table equal? index key)
  (def node-list (vector-ref table index))
  (def found #false)
  (while (and (not found) (not (spartan.base:empty? node-list)))
    (let ((node (first node-list)))
      (if (equal? key (first node))
        (set! found #true)
        (set! node-list (rest node-list)))))
  (if found (first node-list) ()))

(defun find-node-for-removal (table equal? index key)
  (def prev #nil)
  (def node-list (vector-ref table index))
  (def found #false)
  (while (and (not found) (not (spartan.base:empty? node-list)))
    (let ((node (first node-list)))
      (if (equal? key (first node))
        (set! found #true)
        (do
          (set! node-list (rest node-list))
          (set! prev node)))))
  (if found (list (first node-list) prev) ()))

(def insert! #nil) ; forward declaration

(defun resize-to-capacity! (self)
  (def old-table (hashtable-table self))
  (def old-capacity (vector-length old-table))
  (def new-capacity (* 2 old-capacity))
  (def new-table (make-vector new-capacity ()))
  (set-hashtable-table! self new-table)
  (set-hashtable-size! self 0)
  (let ((index 0))
    (while (< index old-capacity)
      (let ((node-list (vector-ref old-table index)))
        (while (not (spartan.base:empty? node-list))
          (let ((node (first node-list)))
            (insert! self (first node) (second node)))
          (set! node-list (rest node-list))))
      (inc! index))))

(defun make-mapping (hash-fn equal-fn)
  (def initial-capacity 2)
  (def table (make-vector initial-capacity ()))
  (make-hashtable hash-fn equal-fn table 0))

(defun mapping? (self)
  (hashtable? self))

(defun empty? (self)
  (= 0 (hashtable-size self)))

(defun size (self)
  (hashtable-size self))

(defun insert! (self key value)
  (match self
    ((record hashtable hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node (find-node table equal-fn index key)))
       (if (not (spartan.base:empty? node))
         ; Key exists, replace current value associated with key
         (set-first! (rest node) value)
         ; Key doesn't exist, add new (key, value) pair to bucket
         (let ((node-list (vector-ref table index))
               (new-node (list key value)))
           (vector-set! table index (adjoin new-node node-list))
           (set-hashtable-size! self (+ 1 size))
           ; Expand table capacity when load factor exceeded
           (if (> (/ size capacity) 0.75)
             (resize-to-capacity! self))))))))

(defun contains? (self key)
  (match self
    ((record hashtable hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node (find-node table equal-fn index key)))
       (not (spartan.base:empty? node))))))

(defun remove! (self key)
  (match self
    ((record hashtable hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node-list (vector-ref table index))
            (node&prev (find-node-for-removal table equal-fn index key)))
       (if (not (spartan.base:empty? node&prev))
         (let ((node (first node&prev))
               (prev (second node&prev)))
           (if (nil? prev)
             (vector-set! table index ())
             (set-rest! prev (rest node)))
           (set-hashtable-size! self (- size 1))))))))

(defun find/default (self key default)
  (match self
    ((record hashtable hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node (find-node table equal-fn index key)))
       (if (spartan.base:empty? node) default (second node))))))

(defun find (self key)
  (find/default self key #nil))

(defun for-each (self proc)
  (match self
    ((record hashtable hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index 0))
       (while (< index capacity)
         (let ((node-list (vector-ref table index)))
           (while (not (spartan.base:empty? node-list))
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
