(defpackage :make-grapher-system
    (:use :cl :asdf))

(in-package :make-grapher-system)

(defsystem "make-grapher"
  :description "MakeGrapher: Makefile finite state automata generation and manipulation utilities."
  :version "0.1"
  :maintainer "Jean-Philippe Barrette-LaPierre <jpb@rrette.com>"
  :licence "MIT"
  :depends-on ("cl-ppcre")
  :components ((:file "make-grapher" :depends-on ("utils"))
               (:file "utils")
               ))
