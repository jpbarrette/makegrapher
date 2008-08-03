(in-package :make-grapher)

(graphviz-export 
 (seed-in (create-graph-from-file "Makefile.complete.mk") "../tmp/build")
 "test.dot")
		 
(graphviz-export (filter-graph (list (seed-in "../tmp/Named_Entity")) *targets*))


(main '("sbcl" "-T" "Makefile.complete.mk" "-o" "test.dot"))

(let ((graph (create-graph-from-file "Makefile.complete.mk")))
  (get-highest-edge graph source (lambda (vertex) (declare (ignore vertex)))))