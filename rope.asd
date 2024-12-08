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
                :components ((:file "basic")
                             (:file "fuzz"))))
  :perform (asdf:test-op
            (o c)
            (multiple-value-bind (stat result)
                (uiop:symbol-call :fiasco :all-tests)
              (print result)
              (assert (eql t stat)))))

(asdf:defsystem #:rope/dev
  :depends-on (#:cl-dot #:rope)
  :components ((:module "dev"
                :components ((:file "package")
                             (:file "graphviz")))))
