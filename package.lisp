(defpackage #:rope
  (:use #:cl)
  (:export
   #:rope                               ; CLASS
   #:rope-length                        ; ACCESSOR
   #:rope-depth                         ; ACCESSOR
   #:branch                             ; CLASS
   #:branch-left                        ; ACCESSOR
   #:branch-right                       ; ACCESSOR
   #:leaf                               ; CLASS
   #:leaf-string                        ; ACCESSOR
   #:make-rope                          ; CONSTRUCTOR
   #:write-rope                         ; FUNCTION
   #:insert-rope                        ; FUNCTION
   #:index-rope                         ; FUNCTION
   #:concat-rope                        ; FUNCTION
   #:split-rope                         ; FUNCTION
   #:kill-rope                          ; FUNCTION
   ))
