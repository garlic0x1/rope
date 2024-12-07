(defpackage #:rope
  (:use #:cl)
  (:export

   #:rope
   #:rope-length
   #:rope-depth

   #:branch
   #:branch-left
   #:branch-right

   #:leaf
   #:leaf-string

   #:rope-length

   #:make-rope
   #:write-rope
   #:concat-rope))
