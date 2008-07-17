
(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 3) (debug 3)))
;(declaim (optimize (speed 3) (space 3) (compilation-speed 0) (safety 0) (debug 0)))

(in-package :make-grapher)

(defstruct (vertex-datum (:conc-name node-))
  (weight 0)
  (distance nil))

;;; ---------------------------------------------------------------------------
(defmethod initialize-vertex-data ((graph cl-graph:basic-graph))
  (let ((vertex-data (make-container 'simple-associative-container)))
    (cl-graph:iterate-vertexes graph (lambda (v) 
				       (setf (item-at vertex-data v) 
					     (make-vertex-datum))))
    (values vertex-data)))

(defgeneric breadth-first-visitor (graph source-vertex fn))

(defmethod breadth-first-visitor ((graph cl-graph:basic-graph) (source cl-graph:basic-vertex) fn)
  ;; initialize
  (let ((vertex-data (initialize-vertex-data graph))
        (queue (make-container 'basic-queue)))
    
    (let ((source-datum (item-at vertex-data source)))
      (setf (node-distance source-datum) 0
            (node-weight source-datum) 1)
      (enqueue queue source)
      
      (loop until (empty-p queue) do
            (let* ((current-vertex (first-item queue))
                   (current (item-at vertex-data current-vertex)))
	      (format t "current vertex: ~A, with datum: ~A~%" current-vertex current)
              (cl-graph:iterate-children current-vertex
                                (lambda (child-vertex)
                                  (let ((child (item-at vertex-data child-vertex)))
				    (format t " looking at child vertex: ~A, with datum: ~A~%" child-vertex child)
                                    (cond
				      ;; the node distance hasn't been assigned
				      ((null (node-distance child))
				       (setf (node-distance child) (+ (node-distance current) 1)
					     (node-weight child) (node-weight current))
				       (enqueue queue child-vertex))

				      ;; the node distance is current distance + 1
				      ((eq (node-distance child) (1+ (node-distance current)))
				       (setf (node-weight child) (+ (node-weight child) (node-weight current))))

				      ;; at this point do nothing
				      )
				    (format t "  updated child: ~A~%" child)
				    )))
              (dequeue queue)
              (funcall fn current-vertex)))
      vertex-data)))


;;; make a simple graph
(defun test ()
  (let ((g (make-container 'cl-graph:graph-container))) 
    (loop for (v1 . v2) in '((a . b) (a . c) (b . d) (c . d) (c . e) (d . f) (e . f) (e . g)) do
	 (cl-graph:add-edge-between-vertexes g v1 v2))
    (let ((source (cl-graph:find-vertex g 'a)))
      (breadth-first-visitor g source (lambda (vertex) (declare (ignore vertex)))))))

(setf my-value (test))

;(print-container (test) t)
  

