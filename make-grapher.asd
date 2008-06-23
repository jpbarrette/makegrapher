(defsystem make-grapher
  :description "MakeGrapher: Makefile finite state automata generation and manipulation utilities."
  :version "0.1.0"
  :maintainer "Jean-Philippe Barrette-LaPierre <jpb@rrette.com>"
  :licence "MIT"
  :depends-on (:cl-ppcre 
  	       :cl-graph 
	       :cl-containers 
	       :getopt	     
	       :moptilities 
	       :metabang-bind
	       :ironclad)
  :components ((:file "make-grapher" :depends-on ("utils"))
               (:file "utils" :depends-on ("package"))
	       (:file "package")))
