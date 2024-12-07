(fiasco:define-test-package :rope/test/basic)
(in-package :rope/test/basic)

(defparameter *string-1*
  "Hello, rope!")

(defparameter *string-2*
  "In computer programming, a rope, or cord, is a data structure
composed of smaller strings that is used to efficiently store and
manipulate longer strings or entire texts. For example, a text
editing program may use a rope to represent the text being edited,
so that operations such as insertion, deletion, and random access
can be done efficiently.")

(deftest string-to-rope ()
  "Test turning strings into ropes, then back into strings."
  (let ((rope-1 (rope:make-rope *string-1*))
        (rope-2 (rope:make-rope *string-2*)))
    (is (= (length *string-1*) (rope:rope-length rope-1)))
    (is (= (length *string-2*) (rope:rope-length rope-2)))
    (is (string= *string-1* (rope:write-rope nil rope-1)))
    (is (string= *string-2* (rope:write-rope nil rope-2)))))
