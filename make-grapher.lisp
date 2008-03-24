;(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (space 3) (compilation-speed 0) (safety 0) (debug 0)))

;(in-package :com.rrette.make-grapher)

(load "utils")

(require 'asdf)
(asdf:operate 'asdf:load-op 'cl-ppcre)
(require 'cl-ppcre)



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
  
(defun graphviz-export-to-file (fsa file) 
  "This function will write the dot description of the FSA in the stream."
  (let ((p (open file :direction :output :if-exists :supersede)))
    (format p "digraph G {~%  rankdir = LR;~%  size = \"8, 11\";~%") 
    (format p "~%~%  rotate=90;~%ratio = auto;~%node [shape = plaintext];~% ")
    (dolist (label (hash-keys fsa))
      (format p " \"~A\"" label))
    (format p ";~%~%")
    (loop for target being the hash-keys in fsa using (hash-value deps) 
          when (not (null deps)) do
          (dolist (dep deps)
            (unless (cl-ppcre:scan *pattern-node-re* dep)
              (format p
                      "  \"~A\" -> \"~A\";~%"
                      target
                      dep))))
    (format p "}~%")
    (close p)
    fsa))
	

(defun graphviz-export (fsa) 
    (graphviz-export-to-file fsa "test.dot"))

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

(defun build-graph (targets pattern-edges)
  "For each target, go through each pattern and check if it matches it."
  (format t "Processing patterns ... ~%")
  (with-hash-table-iterator 
      (my-iterator pattern-edges)
    (loop
       (multiple-value-bind (entry-p target dependencies) (my-iterator)
	 (unless entry-p
	   (return))
	 (let ((*pretty-print* nil))
	   (if (is-pattern target)
	       (expand-target target dependencies targets)
	       (hash-table-update! target targets deps
				   (delete-duplicates (append dependencies deps) :test #'string=))))))))
  
(defun create-graph-from-stream (stream)
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
    (defparameter *pattern-edges* pattern-edges)
    (defparameter *targets* targets)
    (build-graph targets pattern-edges)
    (graphviz-export targets)))


    
(defun create-graph-from-file (file)
  (with-open-file (stream file :direction :input)
    (create-graph-from-stream stream)))


(defun get-dependencies (target targets visited-nodes)
  (multiple-value-bind (deps entry-p) (gethash target visited-nodes)
    (if entry-p
	deps ; we already computed the dependencies
	(progn 
	  (setf (gethash target visited-nodes) nil) ; now it is visited
	  (gethash target targets)))))

(defun update-dependencies (node target visited-nodes)
  (hash-table-update!/default node visited-nodes dependencies nil
			      (delete-duplicates (cons target dependencies) :test #'string=)))


(defun handle-target (path target new-targets visited-nodes paths)
  (multiple-value-bind (value entry-p) (gethash target new-targets)
    (declare (ignore value))
    (when entry-p
      (dolist (node path)
	(update-dependencies node target visited-nodes))
      (hash-table-update!/default (car path) new-targets dependencies nil
				  (delete-duplicates (cons target dependencies) :test #'string=)))
    (unless entry-p
      (setf paths (push (append path (list target)) paths))))
  paths)

(defun seed-in (pattern targets)
  (let ((paths nil)
	(i 0)
	(scanner (cl-ppcre:create-scanner pattern))
	(new-targets (make-hash-table :test #'equal))
	(visited-nodes (make-hash-table :test #'equal)))
    (labels ()
      (with-hash-table-iterator (my-iterator targets)
	(loop (multiple-value-bind (entry-p target) (my-iterator)
		(unless entry-p
		  (return))
		(when (cl-ppcre:scan scanner target)
		  (hash-table-set-if-no-value target new-targets nil)
		  (setf paths (push (list target) paths))))))
      (setf paths (reverse paths))
      (let ((start-node nil))
	(loop 
	   (unless paths (return))
	   (when (not (eq (length paths) (length (remove-duplicates paths :test #'equal))))
	     (break))
	   (incf i)
	   (let* ((path (pop paths))
		  (last-node (car (last path))))
	     (when (not (string= start-node (car path)))
	       (setf start-node (car path))
	       (format t "Processing node ~A ~%" start-node))
					;(format t "~A Processing path ~S~%" i path)
	     (let ((deps (get-dependencies last-node targets visited-nodes)))
	       #|(when (equal last-node "../data/lpa/query-list.bin")
	       (break))|#
	     (dolist (dep deps)
	       (setq paths (handle-target path dep new-targets visited-nodes paths))))))))
    new-targets))
	   


       
	   


