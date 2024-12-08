(defpackage #:rope
  (:use #:cl)
  (:export
   #:rope                               ; CLASS
   #:rope-length                        ; ACCESSOR
   #:make-rope                          ; CONSTRUCTOR
   #:write-rope                         ; FUNCTION
   #:prepend-rope                       ; FUNCTION
   #:append-rope                        ; FUNCTION
   #:insert-rope                        ; FUNCTION
   #:index-rope                         ; FUNCTION
   #:substr-rope                        ; FUNCTION
   #:concat-rope                        ; FUNCTION
   #:split-rope                         ; FUNCTION
   #:kill-rope                          ; FUNCTION
   ))
