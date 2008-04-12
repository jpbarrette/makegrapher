(create-graph-from-file "Makefile.complete.mk")

(build-graph *targets* *pattern-edges*)


(graphviz-export (seed-in "../tmp/build/" *targets*))


(let ((graph (create-graph-from-file "Makefile.complete.mk")))
  (graphviz-export (filter-graph (list (seed-in "../tmp/build/*")) graph)))


(let ((graph (create-graph-from-file "Makefile.complete.mk")))
  (graphviz-export graph))


(graphviz-export (filter-graph (list (seed-in "../tmp/Named_Entity")) *targets*))


(documentation 'seed-in 'function)


(let ((graph (create-graph-from-file "test.complete.mk")))
  (graphviz-export (filter-graph (list (seed-rebuilding-targets)) graph)))
