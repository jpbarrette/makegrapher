
(defparameter *graph* (create-graph-from-file "Makefile.complete.mk"))
(setf (cl-graph:dot-attributes *graph*) '(:rankdir "LR"
					  :size (8 10)
					  :rotate 90
					  :ratio :auto))

(defmethod initialize-instance :after ((vertex cl-graph:dot-vertex-mixin) &key)
  (setf (cl-graph:dot-attributes vertex) '(:shape :plaintext)))

(setf *filtered-graph* (make-projection-graph
			*graph*
			(lambda (v)
			  (search "../tmp/build" (cl-graph:element v)))))



(with-open-file (stream "test.dot" :direction :output :if-exists :supersede)
  (cl-graph:graph->dot *filtered-graph* stream
		       :vertex-labeler (lambda (vertex stream)
					 (format stream "~(~A~)" (cl-graph:element vertex)))
		       :edge-labeler (lambda (edge stream)
				       (declare (ignore stream))
				       (format stream ""))))

*graph*

(build-graph *targets* *pattern-edges*)


(graphviz-export (seed-in "../tmp/build/" *targets*))

(setf *stripped-graph* (filter-graph (list (seed-in "../tmp/Named_Entity")) *graph*))

*stripped-graph*

(let ((graph (create-graph-from-file "Makefile.complete.mk")))
  (graphviz-export (filter-graph (list (seed-in "../tmp/build/*")) graph)))


(let ((graph (create-graph-from-file "Makefile.complete.mk")))
  (graphviz-export graph))


(graphviz-export (filter-graph (list (seed-in "../tmp/Named_Entity")) *targets*))


(documentation 'seed-in 'function)


(setf *other-graph* (let ((graph (create-graph-from-file "test.complete.mk")))
		      (filter-graph (list (seed-all) (seed-rebuilding-targets)) graph)))

(graphviz-export *other-graph*)


