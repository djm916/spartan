(in-module spartan.data.hash-map)

(export make-mapping
        mapping?
        empty?
        size
        contains?
        insert!
        remove!
        find
        find/default
        foreach
        entries
        keys
        values)

(defrecord hashtable (hash-fn equal-fn table size))

(defrecord node (key value next prev))

(defun find-node (table equal? index key)
  (def node (vector-ref table index))
  (def found #false)
  (while (and (not found) (not (nil? node)))
    (if (equal? key (node-key node))
      (set! found #true)
      (set! node (node-next node))))
  (if found node #nil))

(def insert! #nil) ; forward declaration

(defun resize-to-capacity! (self)
  (def old-table (hashtable-table self))
  (def old-capacity (vector-length old-table))
  (def new-capacity (* 2 old-capacity))
  (def new-table (make-vector new-capacity #nil))
  (set-hashtable-table! self new-table)
  (set-hashtable-size! self 0)
  (let ((index 0))
    (while (< index old-capacity)
      (let ((node (vector-ref old-table index)))
        (while (not (nil? node))
          (insert! self (node-key node) (node-value node))
          (set! node (node-next node))))
      (inc! index))))

(defun make-mapping (hash-fn equal-fn)
  (def initial-capacity 2)
  (def table (make-vector initial-capacity #nil))
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
       (if (not (nil? node))
         ; Key exists, replace current value associated with key
         (set-node-value! node value)
         ; Key doesn't exist, add new (key, value) pair to bucket
         (let* ((first-node (vector-ref table index))
                (new-node (make-node key value first-node #nil)))
           (if (not (nil? first-node))
             (set-node-prev! first-node new-node))
           (vector-set! table index new-node)
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
       (not (nil? node))))))

(defun remove! (self key)
  (match self
    ((record hashtable hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node (find-node table equal-fn index key)))
       (if (not (nil? node))
         (let ((next (node-next node))
               (prev (node-prev node)))
           (if (nil? prev)
             (vector-set! table index next)
             (set-node-next! prev next))
           (if (not (nil? next))
             (set-node-prev! next prev))
           (set-hashtable-size! self (- size 1))))))))

(defun find/default (self key default)
  (match self
    ((record hashtable hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index (remainder (hash-fn key) capacity))
            (node (find-node table equal-fn index key)))
       (if (nil? node) default (node-value node))))))

(defun find (self key)
  (find/default self key #nil))

(defun foreach (self proc)
  (match self
    ((record hashtable hash-fn equal-fn table size)
     (let* ((capacity (vector-length table))
            (index 0))
       (while (< index capacity)
         (let ((node (vector-ref table index)))
           (while (not (nil? node))
             (proc (node-key node) (node-value node))
             (set! node (node-next node))))
         (inc! index))))))

(defun entries (self)
  (def result ())
  (foreach self 
    (fun (key value)
      (set! result (adjoin (list key value) result))))
  result)

(defun keys (self)
  (map (fun (pair) (first pair)) (entries self)))

(defun values (self)
  (map (fun (pair) (second pair)) (entries self)))
