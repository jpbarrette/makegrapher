;(declaim (optimize (speed 3) (space 3) (debug 0)))
(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))

(compile-file "package.lisp")
(load "package")
(compile-file "utils.lisp")
(compile-file "make-grapher.lisp")
