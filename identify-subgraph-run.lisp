(setf my-value (make-grapher::test))


(in-package :make-grapher)
(cl-graph:iterate-children 
 (cl-graph:find-vertex (create-graph-from-file "Makefile.complete.mk")
		       "../tmp/build/workspace_built_rtdata")
 (lambda (child-vertex)
   (format t "child: ~A~%" child-vertex)))

(cl-containers:print-container my-value t)
