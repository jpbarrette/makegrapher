(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 3) (debug 3)))
;(declaim (optimize (speed 3) (space 3) (compilation-speed 0) (safety 0) (debug 0)))

;(in-package :com.rrette.make-grapher)

(load "utils")

(require 'asdf)
(asdf:operate 'asdf:load-op 'getopt)
(asdf:operate 'asdf:load-op 'cl-ppcre)
(asdf:operate 'asdf:load-op 'cl-graph)
(asdf:operate 'asdf:load-op 'cl-containers)
(asdf:operate 'asdf:load-op 'moptilities)
(asdf:operate 'asdf:load-op 'metabang-bind)
(asdf:operate 'asdf:load-op 'ironclad)

(require 'getopt)
(require 'cl-ppcre)
(require 'cl-graph)
(require 'cl-containers)
(require 'moptilities)
(require 'ironclad)

(defparameter *pattern-node-re* (cl-ppcre:create-scanner "%"))

(let ((define (cl-ppcre:create-scanner "^define "))
      (reserved-comments '("^# automatic" "^# environment" "^# default" "^# makefile"))
      (endef (cl-ppcre:create-scanner "^endef$"))
      ;;(not-target (cl-ppcre:create-scanner "^# Not a target:$"))
      ;;(start (cl-ppcre:create-scanner "^# Files$")) 
      (append (cl-ppcre:create-scanner ":="))
      (variable-assign (cl-ppcre:create-scanner " = "))
      (data-start (cl-ppcre:create-scanner "^# Make data base"))
      ;;(data-end (cl-ppcre:create-scanner "^# Finished Make data base"))
      ;(skip-next nil)
      (in-data nil)
      ;;(previous-line "")
      (in-define nil))
  (defun process-line (line)
    (when (eq 0 (length line))
      (return-from process-line))
    #|(when skip-next 
      (setf skip-next nil)
      (return-from process-line nil))|#
    (when (and (null in-data) (not (null (cl-ppcre:scan data-start line))))
      (setf in-data t))
    (when (null in-data)
      (return-from process-line nil))
    (when (member line reserved-comments :test #'equal)
      (return-from process-line nil))
    ;; Checking if we are in a define
    (when (not (null (cl-ppcre:scan define line)))
      (setf in-define t))
    (when (not (null (cl-ppcre:scan endef line)))
      (setf in-define nil)
      (return-from process-line nil))
    (when in-define
      (return-from process-line nil))
    ;; skip comments
    (when (eq (char line 0) #\#)
      (return-from process-line nil))
    ;; skip commands
    (when (eq (char line 0)  #\Tab)
      (return-from process-line nil))
    ;; skip if line contains :=
    ;; skip if variable assignment
    (when (cl-ppcre:scan variable-assign line)
      (return-from process-line nil))
    (when (cl-ppcre:scan append line)
      (return-from process-line nil))
    line))


(defun create-graph-creator ()
  (let ((pattern-edges (make-hash-table :test #'equal))
        (non-pattern-edges (make-hash-table :test #'equal)))
    (lambda (line)
      (let* ((answer (split line :char #\:))
             (targets (split (car answer)))
             (dependencies (split (cadr answer))))
        (dolist (target targets)
          (if (is-pattern target)
	      (hash-table-update!/default target pattern-edges deps nil
					  (append deps dependencies))
	      (hash-table-update!/default target non-pattern-edges deps nil
					  (delete-duplicates (append deps dependencies) :test #'string=))))
	(dolist (dep dependencies)
	  (unless (is-pattern dep)
	    (hash-table-set-if-no-value dep non-pattern-edges nil))))
      (values non-pattern-edges pattern-edges))))

(defun create-pattern (target)
  (let ((target target))
    (when (> (position #\% target) 0) ; if % isn't at the beginning, ensure we dont' match a substring.
      (setf target (format nil "^~A" target)))
    (when (not (eql (position #\% target) (- (length  target) 1)))
      (setf target (format nil "~A$" target)))
    (cl-ppcre:create-scanner (string-replace "(.*)" "%" target))))
	 
(defun is-pattern (target)
  (position #\% target))

(defun expand-dep (targets stem dep)
  (let ((dep (string-replace stem "%" dep)))
    (multiple-value-bind (value entry-p) (gethash dep targets)
      (declare (ignore value))
      (if entry-p
	  dep
	  nil))))
    

(defun expand-deps (targets stem deps)
  (let ((expanded-deps nil))
    (dolist (dep deps)
      (if (is-pattern dep)
	  (let ((expanded-dep (expand-dep targets stem dep)))
	    (when (setf expanded-deps (cons expanded-dep expanded-deps))))
	  (setf expanded-deps (cons dep expanded-deps))))
    expanded-deps))

(defun match-pattern (targets target deps)
  (if (not (is-pattern target))
      (list target)
      (let ((scanner (create-pattern target))
	    (matched-patterns nil))
	(with-hash-table-iterator
	    (my-iterator targets)
	  (loop
	     (multiple-value-bind (entry-p key) (my-iterator)
	       (unless entry-p
		 (return))
	       (multiple-value-bind (val stem) (cl-ppcre:scan-to-strings scanner key)
		 (when val
		   (let ((deps (expand-deps targets (aref stem 0) deps)))
		     (setf matched-patterns (cons (cons key deps) matched-patterns))))))
	  matched-patterns)))))


(defun expand-target (target deps targets)
  (when deps
    (let ((matched-targets (match-pattern targets target deps)))
      (dolist (matched-target matched-targets)
	(let ((target (car matched-target))
	      (stem (cdr matched-target)))
	  (format t "~S stem:~S~%" target stem))))))

(defun build-graph (targets pattern-edges graph-type)
  "For each target, go through each pattern and check if it matches it."
  (format t "Processing patterns ... ~%")
  (let ((graph (cl-graph::make-container 
		graph-type
		:vertex-test #'equal
		:default-edge-type :directed)))
    (with-hash-table-iterator 
	(my-iterator pattern-edges)
      (loop
	 (multiple-value-bind (entry-p target dependencies) (my-iterator)
	   (unless entry-p
	     (return))
	   (let ((*pretty-print* nil))
	     (declare (special *pretty-print*))
	     (if (is-pattern target)
		 (expand-target target dependencies targets)
		 (hash-table-update! target targets deps
				     (delete-duplicates (append dependencies deps) :test #'string=)))))))
    (with-hash-table-iterator 
	(my-iterator targets)
      (loop 
	   (multiple-value-bind (entry-p target dependencies) (my-iterator)
	     (unless entry-p
	       (return))
	     (dolist (dep dependencies)
	       (cl-graph:add-edge-between-vertexes graph target dep)))))
    graph))

(defparameter *graph-cache* (make-hash-table :test #'equal) 
  "This is the graph cache, by digest of streams")

(defmethod create-graph-from-file :around ((s string) &key graph-type)
  (declare (ignore graph-type))
  (let* ((digest (ironclad:byte-array-to-hex-string 
		  (ironclad:digest-file :sha1 s)))
	 (graph (gethash digest *graph-cache*)))
    (unless graph
      (setf graph (call-next-method))
      (setf (gethash digest *graph-cache*) graph))
    graph))

(defmethod create-graph-from-stream (stream &key (graph-type 'cl-graph:dot-graph))
  (let ((graph-creator (create-graph-creator))
	(targets nil)
	(pattern-edges nil))
    (format t "Reading makefile ...~%")
    (for-each-line-in-stream (line stream)
      (let ((new-line (process-line line)))
	(when new-line
	  (multiple-value-bind (trgt pe)
	      (apply graph-creator (list line))
	    (setf targets trgt)
	    (setf pattern-edges pe)))))
    (build-graph targets pattern-edges graph-type)))

(defmethod create-graph-from-stream :around ((s string) &key graph-type)
  (declare (ignore s graph-type))
  (call-next-method))
    
(defmethod create-graph-from-file (file &key (graph-type 'cl-graph:dot-graph))
  (with-open-file (stream file :direction :input)
    (create-graph-from-stream stream :graph-type graph-type)))


(defun regex-seed-in (graph pattern)
  "This function will seed in the graph all targets that matches 
the given regex pattern"
  (let ((scanner (cl-ppcre:create-scanner pattern)))
    (make-projection-graph graph (lambda (v) (cl-ppcre:scan scanner v)))))

(defun seed-in (graph text)
  "This function will seed in the graph all targets that contains the given text"
  (make-projection-graph graph (lambda (v) (search text (cl-graph:element v)))))

#|(defun seed-rebuilding-targets ()
  "This function will seed any dependency for which target is done, 
but is gonna to be built again because of one of them."
  (lambda (target graph)
    (let ((deps nil))
      (when (probe-file target)
	(dolist (dep (gethash target (makefile-graph-targets graph)))
	  (when (or (not (probe-file dep)) 
		    (> (file-write-date dep) (file-write-date target)))
	    (hash-table-update!/default dep (makefile-graph-properties graph) properties nil
					(append (list (cons "fontcolor" "red")) properties))
	    (setf deps (cons dep deps)))))
      deps)))|#
      

(defmethod make-projection-graph (old-graph
				  test-fn
				  &key
				  (new-graph 
				   (moptilities:copy-template old-graph)))
  (let ((paths nil)
	(visited-graph (cl-graph::make-container 'cl-graph:dot-graph ; This will keep visited node visited.
						   :vertex-test #'equal
						   :default-edge-type :directed)))
    (cl-graph:iterate-vertexes old-graph
			       (lambda (vertex)
				 (when (funcall test-fn vertex)
				   (setf paths (push (list vertex) paths))
				   (cl-graph:add-vertex new-graph (cl-graph:element vertex)))))
    (loop (unless paths (return)) ; loop until we visited all the paths.
       (let* ((path (pop paths))
	      (first-vertex (car path))
	      (current-vertex first-vertex)
	      (last-vertex (car (last path)))
	      (deps (cl-graph:child-vertexes last-vertex)))
	 (when (not (eq current-vertex first-vertex))
	   (format t "current vertex: ~A~%" first-vertex)
	   (setf current-vertex first-vertex)
	   (break))
	 (dolist (dep deps)
	   (if-bind (visited-vertex (cl-graph:find-vertex visited-graph (cl-graph:element dep) nil))
		    ;; the node was visited, the remote vertices should be the filtered vertices
		    (cl-graph:iterate-edges visited-vertex 
					    (lambda (edge)
					      (cl-graph:add-edge-between-vertexes new-graph
										  (cl-graph:vertex-1 edge) (cl-graph:vertex-2 edge))))
		    ;; the node ins't visited yet.
		    (let ((vertex (cl-graph:find-vertex new-graph (cl-graph:element dep) nil)))
		      (cl-graph:add-vertex visited-graph (cl-graph:element dep))
		      (cond (vertex
			     (cl-graph:add-edge-between-vertexes new-graph (cl-graph:element first-vertex) (cl-graph:element vertex))
			     (dolist (path-vertex (cdr path))
			       (cl-graph:add-edge-between-vertexes visited-graph (cl-graph:element path-vertex) (cl-graph:element vertex))))
			    (t 
			     (setf paths (push (append path (list dep)) paths))
			     #|(break)|#))))))))
  new-graph)
		    

(defmethod initialize-instance :after ((vertex cl-graph:dot-vertex-mixin) &key)
    (setf (cl-graph:dot-attributes vertex) '(:shape :plaintext)))

(defun graphviz-export (graph filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (setf (cl-graph:dot-attributes graph) '(:rankdir "LR"
					  :size (8 10)
					  :rotate 90
					  :ratio :auto))
    (cl-graph:graph->dot graph stream
			 :vertex-labeler (lambda (vertex stream)
					   (format stream "~(~A~)" (cl-graph:element vertex)))
			 :edge-labeler (lambda (edge stream)
					 (declare (ignore stream edge))))))


(defun main (argv)
  (multiple-value-bind (args opts errors) 
      (getopt:getopt argv '(("T" :required)
				    ("o" :required)))
    (let ((makefile-stream t))
      (dolist (o opts)
	(cond ((eql "T" (first o))
	       (setf makefile-stream (second o))))
	(format t "opts:~S~%args:~S~%errors:~S~%" opts args errors)))))




	  

	   


       
	   


