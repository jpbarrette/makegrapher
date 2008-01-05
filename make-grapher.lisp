(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 3) (debug 3)))

(in-package :com.rrette.make-grapher)

(load "utils.lisp")

(require 'asdf)
;(require 'asdf-install)
;(asdf-install:install 'cl-ppcre)
(asdf:operate 'asdf:load-op 'cl-ppcre)
(require 'cl-ppcre)




(let ((define (cl-ppcre:create-scanner "^define "))
      ;;(reserved-comments '("^# automatic" "^# environment" "^# default" "^# makefile"))
      (endef (cl-ppcre:create-scanner "^endef$"))
      ;;(not-target (cl-ppcre:create-scanner "^# Not a target:$"))
      ;;(start (cl-ppcre:create-scanner "^# Files$")) 
      (append (cl-ppcre:create-scanner ":="))
      (variable-assign (cl-ppcre:create-scanner " = "))
      (data-start (cl-ppcre:create-scanner "^# Make data base"))
      ;;(data-end (cl-ppcre:create-scanner "^# Finished Make data base"))
      (phony (cl-ppcre:create-scanner "^.PHONY: "))
      (skip-next nil)
      (in-data nil)
      ;;(previous-line "")
      (in-define nil))
  (defun register-phony (line)
    (multiple-value-bind (start end)  (cl-ppcre:scan phony line)
      (declare (ignore end))
      (when (null start)
	nil)
      nil))
  (defun process-line (line)
    (when (eq 0 (length line))
      (return-from process-line))
    (when skip-next 
      (setf skip-next nil)
      (return-from process-line nil))
    (when (and (null in-data) (not (null (cl-ppcre:scan data-start line))))
      (setf in-data t))
    (when (null in-data)
      (return-from process-line nil))
    ;; 	       (when (member line reserved-comments :test #'equal)
    ;; 		 (return-from process-line nil))
    ;; Checking if we are in a define
    (when (not (null (cl-ppcre:scan define line)))
      (setf in-define t))
    (when (not (null (cl-ppcre:scan endef line)))
      (setf in-define nil)
      (return-from process-line nil))
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
      (return-from process-line nil))
    ;; skip if line contains :=
    ;; skip if variable assignment
    (when (cl-ppcre:scan variable-assign line)
      (return-from process-line nil))
    (when (cl-ppcre:scan append line)
      (return-from process-line nil))
    line))


(defun create-graph-from-stream (stream)
  (let ((lines nil)
	(line-no 0))
    (for-each-line-in-stream (line stream)
      (let ((new-line (process-line line)))
	(when new-line
	  (format t "~A~%" new-line)
	  (incf line-no)
	  (setf lines (cons new-line lines)))))
    lines))

    
(defun create-graph-from-file (file)
  (with-open-file (stream file :direction :input)
    (create-graph-from-stream stream)))

;;(create-graph-from-file "Makefile.complete.mk")


