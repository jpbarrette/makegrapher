(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 3) (debug 3)))

;(in-package :com.rrette.make-grapher)

(load "utils")

(require 'asdf)
;(require 'asdf-install)
;(asdf-install:install 'cl-ppcre)
(asdf:operate 'asdf:load-op 'cl-ppcre)
(require 'cl-ppcre)




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



(let (#|(phony-re (cl-ppcre:create-scanner "^.PHONY: "))|#
      (pattern-node-re (cl-ppcre:create-scanner "%")))
  (defun create-graph-creator ()
    (let ((pattern-edges nil)
	  (non-pattern-edges (make-hash-table)))
      (lambda (line)
	(let* ((answer (split line :char #\:))
	       (targets (split (car answer)))
	       (dependencies (split (cadr answer))))
	  (dolist (target targets)
	    (if (cl-ppcre:scan pattern-node-re target)
		(setf pattern-edges (cons (list target dependencies) pattern-edges))
		(progn 
		  (dolist (dep dependencies)
		    (if (cl-ppcre:scan pattern-node-re target)
			(setf pattern-edges (cons (list target dep) pattern-edges))
			(hash-table-update! target non-pattern-edges deps
					    (append deps dependencies)))))))
	  (values non-pattern-edges pattern-edges))))

  (defun graphviz-export-to-file (fsa file) 
    "This function will write the dot description of the FSA in the stream."
    (let ((p (open file :direction :output :if-exists :supersede)))
      (format p "digraph G {~%  rankdir = LR;~%  size = \"8, 10\";~%") 
      (format p "~%~%  node [shape = circle];~% ")
      (dolist (label (hash-keys fsa))
	(format p " \"~A\"" label))
      (format p ";~%~%")
      (loop for target being the hash-keys in fsa using (hash-value deps) 
	 when (not (null deps)) do
	 (dolist (dep deps)
	   (unless (cl-ppcre:scan pattern-node-re dep)
	     (format p
		     "  \"~A\" -> \"~A\";~%"
		     target
		     dep))))
      (format p "}~%")
      (close p)
      fsa)))
	

(defun graphviz-export (fsa) 
    (graphviz-export-to-file fsa "test.dot"))

(defun create-pattern (target)
  (cl-ppcre:create-scanner (string-replace ".*" "%" target)))
	 
(defun update-patterns (patterns-hash target)
  (when (position #\% target) 
    (if (null (gethash target patterns-hash))
	(setf (gethash target patterns-hash) (create-pattern target)))))

(defun build-graph (targets pattern-edges)
  "For each target, go through each pattern and check "
  "if it matches it."
  (dolist (pattern-edge pattern-edges)
    (let ((target (car pattern-edge))
	  (deps (cadr pattern-edge)))
      (update-patterns target)
      (dolist (dep deps)
	(update-patterns dep))))
  (with-hash-table-iterator
      (my-iterator targets)
    (loop
	 (multiple-value-bind (entry-p key value)
	     (my-iterator)
	   (dolist (pattern-edge pattern-edges)
	     )))))
       

(defun create-graph-from-stream (stream)
  (let ((graph-creator (create-graph-creator))
	(targets nil)
	(pattern-edges nil))
    (for-each-line-in-stream (line stream)
      (let ((new-line (process-line line)))
	(when new-line
	  (multiple-value-bind (trgt pe)
	      (apply graph-creator (list line))
	    (setf targets trgt)
	    (setf pattern-edges pe)))))
    (build-graph targets pattern-edges)
    (graphviz-export targets)))

    
(defun create-graph-from-file (file)
  (with-open-file (stream file :direction :input)
    (create-graph-from-stream stream)))

;;(create-graph-from-file "Makefile.complete.mk")


