(asdf:defsystem #:rope
  :author "garlic0x1"
  :license "MIT"
  :depends-on ()
  :components ((:file "package")
               (:file "rope"))
  :in-order-to ((test-op (test-op #:rope/test))))

(asdf:defsystem #:rope/test
  :depends-on (#:rope #:fiasco)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "basic"))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :fiasco :all-tests)))

(asdf:defsystem #:rope/dev
  :depends-on (#:rope #:cl-dot)
  :components ((:module "dev"
                :components ((:file "package")
                             (:file "graphviz")))))
