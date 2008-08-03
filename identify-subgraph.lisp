(in-package :make-grapher)


(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 3) (debug 3)))
;(declaim (optimize (speed 3) (space 3) (compilation-speed 0) (safety 0) (debug 0)))

(in-package :make-grapher)

(defstruct (vertex-datum (:conc-name node-))
  (weight 0)
  (distance nil))

(defstruct (edge-datum (:conc-name edge-))
  (score 0))


;;; ---------------------------------------------------------------------------
(defmethod initialize-vertex-data ((graph cl-graph:basic-graph))
  (let ((vertex-data (make-container 'associative-container)))
    (cl-graph:iterate-vertexes graph (lambda (v) 
				       (setf (item-at vertex-data v) 
					     (make-vertex-datum))))
    (values vertex-data)))

(defgeneric initialize-edge-data (graph))

(defmethod initialize-edge-data ((graph cl-graph:basic-graph))
  (let ((data (make-container 'associative-container)))
    (cl-graph:iterate-edges graph (lambda (e) 
				    (setf (item-at data e) 
					  (make-edge-datum))))
    (values data)))

(defgeneric get-edges-score-for-vertex (graph source-vertex))

(defmethod get-edges-score-for-vertex ((graph cl-graph:basic-graph) (source cl-graph:basic-vertex))
  ;; initialize
  (let ((vertex-data (initialize-vertex-data graph))
	(edge-data (initialize-edge-data graph))
        (queue (make-container 'basic-queue))
	(highest-score 0)
	(highest-edge nil))
    (let ((source-datum (item-at vertex-data source))
	  (leaves (make-container 'basic-queue))
	  (farthest-vertices (make-container 'stack-container)))
      (setf (node-distance source-datum) 0
            (node-weight source-datum) 1)
      (enqueue queue source)
      
      (loop until (empty-p queue) do
            (let* ((current-vertex (first-item queue))
                   (current (item-at vertex-data current-vertex)))
	      (format t "current vertex: ~A, with datum: ~A~%" current-vertex current)
	      
	      ;; if it's a leave add it in leaves, otherwise add it to the farthest-vertices
	      (if (<= (vertex-degree current-vertex) 1)
		  (enqueue leaves current-vertex)
		  (insert-item farthest-vertices current-vertex))
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
              (dequeue queue)))
      ;; Setting leaves's edge score
      (format t "Setting the leaves's edge betweeness~%")
      (loop until (empty-p leaves) do
            (let* ((current-vertex (first-item leaves))
                   (current (item-at vertex-data current-vertex)))
	      (format t "current vertex: ~A, with datum: ~A~%" current-vertex current)
              (iterate-edges current-vertex
			    (lambda (edge)
			      (let* ((child-vertex (other-vertex edge current-vertex))
				     (child (item-at vertex-data child-vertex))
				     (edge-datum (item-at edge-data edge)))
				(format t " looking at child vertex: ~A, with datum: ~A~%" child-vertex child)
				(setf (edge-score edge-datum) (/ (node-weight child) (node-weight current)))))))
	   (dequeue leaves))
      (format t "Setting the leaves's edge betweeness for other than leaves~%")
      (loop until (empty-p farthest-vertices) do
	   (let* ((current-vertex (first-item farthest-vertices))
		  (current (item-at vertex-data current-vertex))
		  (below-score 1))
	     ;; collecting the sum of score which is 1 + scores of neighbors edges below
	     (iterate-edges current-vertex 
			    (lambda (edge)
			      (let* ((child-vertex (other-vertex edge current-vertex))
				     (child (item-at vertex-data child-vertex))
				     (edge-datum (item-at edge-data edge)))
				(when (> (node-distance child) (node-distance current))
				  (setf below-score (+ below-score (edge-score edge-datum)))))))
	     ;; we got the below score
	     (iterate-edges current-vertex
			    (lambda (edge)
			      (let* ((child-vertex (other-vertex edge current-vertex))
				     (child (item-at vertex-data child-vertex))
				     (edge-datum (item-at edge-data edge)))
				(when (< (node-distance child) (node-distance current))
				  (setf (edge-score edge-datum) (* below-score (/ (node-weight child) (node-weight current))))
				  (when (< highest-score (edge-score edge-datum))
				    (setf highest-score (edge-score edge-datum)
					  highest-edge edge)))))))
				    
	   (pop-item farthest-vertices)))
      highest-edge))

(defgeneric get-highest-edge (graph source-vertex))

(defmethod get-highest-edge (graph source-vertex)
  (iterate-vertexes 
   graph
   (lambda (vertex)
     (




(defgeneric remove-most-critical-edge (graph source))

(defmethod remove-most-critical-edge (graph source)
  (delete-edge graph (get-highest-edge graph source)))

;;; make a simple graph
(defun test ()
  (let ((g (make-container 'cl-graph:dot-graph))) 
    (loop for (v1 . v2) in '((a . b) (a . c) (b . d) (c . d) (c . e) (d . f) (e . f) (e . g)) do
	 (cl-graph:add-edge-between-vertexes g v1 v2))
    (let ((source (cl-graph:find-vertex g 'a)))
      (remove-most-critical-edge g source)
      (graphviz-export g "test.dot"))))

  
;(fmakunbound 'get-highest-edge)
