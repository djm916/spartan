(in-module spartan.data.hash-map)

(export mapping
        mapping?
        empty?
        size
        contains?
        insert!
        remove!
        find
        foreach
        entries
        keys
        values)

(defrecord <mapping>
  make-mapping
  mapping?
  (hash-fn mapping-hash-fn)
  (equal-fn mapping-equal-fn)
  (entries mapping-entries mapping-set-entries!)
  (size mapping-size mapping-set-size!))

(defrecord <node>
  make-node
  node?
  (key node-key)
  (value node-value node-set-value!)
  (next node-next node-set-next!)
  (prev node-prev node-set-prev!))

(defun find-node (entries equal? index key)
  (def node (vector-ref entries index))
  (def found #false)
  (while (and (not found) (not (nil? node)))
    (if (equal? key (node-key node))
      (set! found #true)
      (set! node (node-next node))))
  (if found node #nil))

(def insert! #nil) ; forward declaration

(defun resize-to-capacity! (self)
  (def old-entries (mapping-entries self))
  (def old-length (vector-length old-entries))
  (def new-length (* 2 old-length))
  (mapping-set-entries! self (make-vector new-length #nil))
  (mapping-set-size! self 0)
  (let ((index 0))
    (while (< index old-length)
      (let ((node (vector-ref old-entries index)))
        (while (not (nil? node))
          (insert! self (node-key node) (node-value node))
          (set! node (node-next node))))
      (inc! index))))

(defun mapping (hash-fn equal-fn :rest kvpairs)
  (def initial-capacity 2)
  (def entries (make-vector initial-capacity #nil))
  (let ((m (make-mapping hash-fn equal-fn entries 0)))
    (spartan.base:foreach (fun (e) (insert! m (first e) (second e))) kvpairs)
    m))

(defun empty? (self)
  (= 0 (mapping-size self)))

(defun size (self)
  (mapping-size self))

(set! insert! (fun (self key value)
  (match self
    ((record <mapping> hash-fn equal-fn entries size)
     (let* ((capacity (vector-length entries))
            (index (remainder (hash-fn key) capacity))
            (node (find-node entries equal-fn index key)))
       (if (not (nil? node))
         ; Key exists, replace current value associated with key
         (node-set-value! node value)
         ; Key doesn't exist, add new (key, value) pair to bucket
         (let* ((first-node (vector-ref entries index))
                (new-node (make-node key value first-node #nil)))
           (if (not (nil? first-node))
             (node-set-prev! first-node new-node))
           (vector-set! entries index new-node)
           (mapping-set-size! self (+ 1 size))
           ; Expand table capacity when load factor exceeded
           (if (> (/ size capacity) 0.75)
             (resize-to-capacity! self)))))))))

(defun contains? (self key)
  (match self
    ((record <mapping> hash-fn equal-fn entries size)
     (let* ((capacity (vector-length entries))
            (index (remainder (hash-fn key) capacity))
            (node (find-node entries equal-fn index key)))
       (not (nil? node))))))

(defun remove! (self key)
  (match self
    ((record <mapping> hash-fn equal-fn entries size)
     (let* ((capacity (vector-length entries))
            (index (remainder (hash-fn key) capacity))
            (node (find-node entries equal-fn index key)))
       (if (not (nil? node))
         (let ((next (node-next node))
               (prev (node-prev node)))
           (if (nil? prev)
             (vector-set! entries index next)
             (node-set-next! prev next))
           (if (not (nil? next))
             (node-set-prev! next prev))
           (mapping-set-size! self (- size 1))))))))

(defun find (self key :option (default #nil))
  (match self
    ((record <mapping> hash-fn equal-fn entries size)
     (let* ((capacity (vector-length entries))
            (index (remainder (hash-fn key) capacity))
            (node (find-node entries equal-fn index key)))
       (if (nil? node) default (node-value node))))))

(defun foreach (self proc)
  (match self
    ((record <mapping> hash-fn equal-fn entries size)
     (let* ((capacity (vector-length entries))
            (index 0))
       (while (< index capacity)
         (let ((node (vector-ref entries index)))
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
