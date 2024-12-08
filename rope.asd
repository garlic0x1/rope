(asdf:defsystem #:rope
  :description "Immutable Ropes for Common Lisp"
  :author "garlic0x1"
  :license "MIT"
  :components ((:file "package")
               (:file "rope"))
  :in-order-to ((test-op (test-op #:rope/test))))

(asdf:defsystem #:rope/test
  :depends-on (#:alexandria #:fiasco #:rope)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "basic"))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :fiasco :all-tests)))

(asdf:defsystem #:rope/dev
  :depends-on (#:cl-dot #:rope)
  :components ((:module "dev"
                :components ((:file "package")
                             (:file "graphviz")))))
