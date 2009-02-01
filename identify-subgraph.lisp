(in-package :make-grapher)
;(require :mathstats)
(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 3) (debug 3)))
;(declaim (optimize (speed 3) (space 3) (compilation-speed 0) (safety 0) (debug 0)))

;(in-package :make-grapher)

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

(defgeneric get-edges-score-for-vertex (graph source-vertex) )
;;   :documentation "Returns a hash-table with edges 
;; has keys and edge-datum as values. The second value is 
;; the highest scored edge")

(defmethod get-edges-score-for-vertex ((graph cl-graph:basic-graph) (source cl-graph:basic-vertex))
  ;; initialize
  (let ((vertex-data (initialize-vertex-data graph))
	(edge-data (initialize-edge-data graph))
        (queue (make-container 'basic-queue)))
    (let ((source-datum (item-at vertex-data source))
	  (leaves (make-container 'basic-queue))
	  (farthest-vertices (make-container 'stack-container)))
      (format t "processing current vertex: ~A~%" source)
      (setf (node-distance source-datum) 0
            (node-weight source-datum) 1)
      (enqueue queue source)
      
      (loop until (empty-p queue) do
            (let* ((current-vertex (first-item queue))
                   (current (item-at vertex-data current-vertex)))
	      ;(format t "current vertex: ~A, with datum: ~A~%" current-vertex current)
	      
	      ;; if it's a leave add it in leaves, otherwise add it to the farthest-vertices
	      (if (<= (vertex-degree current-vertex) 1)
		  (enqueue leaves current-vertex)
		  (insert-item farthest-vertices current-vertex))
              (cl-graph:iterate-children current-vertex
                                (lambda (child-vertex)
                                  (let ((child (item-at vertex-data child-vertex)))
				    #|(format t " looking at child vertex: ~A, with datum: ~A~%" child-vertex child)|#
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
				    #|(format t "  updated child: ~A~%" child)|#
				    )))
              (dequeue queue)))
      ;; Setting leaves's edge score
      #|(format t "Setting the leaves's edge betweeness~%")|#
      (loop until (empty-p leaves) do
            (let* ((current-vertex (first-item leaves))
                   (current (item-at vertex-data current-vertex)))
	      #|(format t "current vertex: ~A, with datum: ~A~%" current-vertex current)|#
              (iterate-edges current-vertex
			    (lambda (edge)
			      (let* ((child-vertex (other-vertex edge current-vertex))
				     (child (item-at vertex-data child-vertex))
				     (edge-datum (item-at edge-data edge)))
				#|(format t " looking at child vertex: ~A, with datum: ~A~%" child-vertex child)|#
				(setf (edge-score edge-datum) (/ (node-weight child) (node-weight current)))))))
	   (dequeue leaves))
      #|(format t "Setting the leaves's edge betweeness for other than leaves~%")|#
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
				(labels ((distance (v)
					   (if (null (node-distance v))
					       0
					       (node-distance v))))
				  (when (> (distance child) (distance current))
				    (setf below-score (+ below-score (edge-score edge-datum))))))))
	     ;; we got the below score
	     (iterate-edges current-vertex
			    (lambda (edge)
			      (let* ((child-vertex (other-vertex edge current-vertex))
				     (child (item-at vertex-data child-vertex))
				     (edge-datum (item-at edge-data edge)))
				(labels ((distance (v)
					   (if (null (node-distance v))
					       0
					       (node-distance v))))
				  (when (< (distance child) (distance current))
				    (setf (edge-score edge-datum) (* below-score (/ (node-weight child) (node-weight current))))))))))
	   (pop-item farthest-vertices)))
    edge-data))

;(defgeneric get-highest-edge (graph))

(defmethod get-highest-edge ((graph cl-graph:basic-graph))
  (let ((total-edges-score (make-hash-table :test #'equal))
	(highest-score 0)
	(highest-edge nil))
    (iterate-vertexes 
     graph
     (lambda (vertex)
       (let ((edges-scores (get-edges-score-for-vertex graph vertex)))
	 (iterate-key-value edges-scores 
			    (lambda (edge edge-datum)
			      (hash-table-update! (edge total-edges-score e-datum :default 0)
				(+ e-datum (edge-score edge-datum))))))))
    (with-hash-table-iterator 
	(my-iterator total-edges-score)
      (loop
	 (multiple-value-bind (entry-p edge edge-datum) (my-iterator)
	   (unless entry-p
	     (return))
	   #|(format t "edge score: ~A => ~A~%" edge edge-datum)|#
	   (when (and edge-datum (> edge-datum highest-score))
	     (setf highest-score edge-datum
		   highest-edge edge)))))
    highest-edge))

(defmethod get-start-highest-edge ((graph cl-graph:basic-graph))
  (let ((total-edges-score (make-hash-table :test #'equal))
	(highest-score 0)
	(highest-edge nil)
	(source (cl-graph:find-vertex graph "../tmp/build/workspace_built_rtdata")))
    (let ((edges-scores (get-edges-score-for-vertex graph source)))
      (iterate-key-value edges-scores 
			 (lambda (edge edge-datum)
			   (hash-table-update! (edge total-edges-score e-datum :default 0)
			     (+ e-datum (edge-score edge-datum))))))
    (with-hash-table-iterator 
	(my-iterator total-edges-score)
      (loop
	 (multiple-value-bind (entry-p edge edge-datum) (my-iterator)
	   (unless entry-p
	     (return))
	   #|(format t "edge score: ~A => ~A~%" edge edge-datum)|#
	   (when (and edge-datum (> edge-datum highest-score))
	     (setf highest-score edge-datum
		   highest-edge edge)))))
    highest-edge))


(defgeneric remove-most-critical-edge (graph))

(defmethod remove-most-critical-edge (graph)
  (let ((most-critical-edge (get-start-highest-edge graph)))
    (delete-edge graph most-critical-edge)
    most-critical-edge))

;;; make a simple graph
(defun test ()
  (let ((g (create-graph-from-file "Makefile.complete.mk")))
    (format t "critical edge: ~A~%" (remove-most-critical-edge g))
    (graphviz-export g "test.dot")))

  
;(fmakunbound 'remove-most-critical-edge)
