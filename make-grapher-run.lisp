(create-graph-from-file "Makefile.complete.mk")

(build-graph *targets* *pattern-edges*)


(graphviz-export (seed-in "../tmp/build/" *targets*))


(let ((graph (create-graph-from-file "Makefile.complete.mk")))
  (graphviz-export (seed-in "../tmp/build/" graph)))