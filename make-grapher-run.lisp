(create-graph-from-file "Makefile.complete.mk")

(build-graph *targets* *pattern-edges*)


(graphviz-export (seed-in "../tmp/build/" *targets*))

(setf *graph* (create-graph-from-file "Makefile.complete.mk"))
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


