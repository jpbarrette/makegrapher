(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))

(load "utils.lisp")

(require 'asdf)
(asdf:operate 'asdf:load-op 'cl-ppcre)
(require 'cl-ppcre)


(let ((my-scanner (cl-ppcre:create-scanner "a?b")))
  (cl-ppcre:scan my-scanner "aabaa"))

;# Pattern-specific Variable Values
;# Directories
;# Files
;# Variables


(require 'cl-ppcre)


(defun create-graph-from-stream (stream)
  (let ((reserved-comments '("^# automatic" "^# environment" "^# default" "^# makefile"))
	(define (cl-ppcre:create-scanner "^define "))
	(endef (cl-ppcre:create-scanner "^endef$"))
	(not-target (cl-ppcre:create-scanner "^# Not a target:$"))
	(start (cl-ppcre:create-scanner "^# Files$")) 
	(not-append (cl-ppcre:create-scanner "^\S+.*:="))
	(p (cl-ppcre:create-scanner "^\S+.*:[^=]'"))
	(not-variable-assign (cl-ppcre:create-scanner " = "))
        (data-start (cl-ppcre:create-scanner "^# Make data base"))
        (data-end (cl-ppcre:create-scanner "^# Finished Make data base"))
	(phony (cl-ppcre:create-scanner "^.PHONY: "))
	(lines nil)
	(skip-next nil)
	(in-data nil)
	(line "")
	(previous-line "")
	(in-define nil))
    (labels ((register-phony (line)
	       (multiple-value-bind (start end)  (cl-ppcre:scan phony line)
		 (declare (ignore end))
		 (when (null start)
		   nil)
		 nil))
	     (process-line (line)
	       (when (eq 0 (length line))
		 (return-from process-line))
	       (when skip-next 
		 (setf skip-next nil)
		 (return-from process-line nil))
	       (when (and (null in-data) (not (null (cl-ppcre:scan data-start line))))
	       (setf in-data t))
	       (when (null in-data)
	       (return-from process-line nil))
	       (when (member line reserved-comments :test #'string=)
	       (return-from process-line nil))
	       ;; Checking if we are in a define
	       (when (not (null (cl-ppcre:scan define line)))
	       (setf in-define t))
	       (when (not (null (cl-ppcre:scan endef line)))
	       (setf in-define nil))
	       (when in-define
	       (return-from process-line nil))
	       ;; PHONY processing
	       (when (not (null (register-phony line)))
	       (return-from process-line nil))
	       ;; skip comments
	       (when (eq (char line 0) #\#)
	       (return-from process-line nil))
	       ;; skip commands
	       (when (eq (char line 0)  #\Tab)
	       (return-from process-line nil)))
	     (let ((non-filtered-lines nil))
	       (for-each-line-in-stream (line stream)
		 (let ((new-line (process-line line)))
		   (when new-line
		     (setf non-filtered-lines (cons new-line non-filtered-lines)))))
	       non-filtered-lines)))))



(defun create-graph-from-file (file)
  (with-open-file (stream file :direction :input)
    (create-graph-from-stream stream)))

(create-graph-from-file "Makefile.complete.mk")


