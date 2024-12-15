(defpackage #:rope/dev
  (:use #:cl #:cl-dot #:rope)
  (:local-nicknames
   (#:a #:alexandria))
  (:export
   ;; graphviz.lisp
   #:graph-ropes
   ;; benchmark.lisp
   #:benchmark-insert
   ))
