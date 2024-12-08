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
    (is (string= *string-1* (rope:write-rope rope-1 nil)))
    (is (string= *string-2* (rope:write-rope rope-2 nil)))))

(deftest split ()
  "Test splitting ropes and check to ensure it is the same as splitting strings."
  (dotimes (i (length *string-2*))
    (let ((rope (rope:make-rope *string-2*)))
      (multiple-value-bind (ante post) (rope:split-rope rope i)
        (is (string= (subseq *string-2* 0 i) (rope:write-rope ante nil)))
        (is (string= (subseq *string-2* i) (rope:write-rope post nil)))))))

(deftest delete-and-insert ()
  "Make a rope, then a rope with a part deleted, then inserted."
  (let* ((rope (rope:make-rope *string-1*))
         (killed (rope:kill-rope rope 0 5))
         (inserted (rope:insert-rope killed 0 "Goodbye"))
         (super (rope:insert-rope rope 7 "super ")))
    (is (string= "Hello, rope!" (rope:write-rope rope nil)))
    (is (string= ", rope!" (rope:write-rope killed nil)))
    (is (string= "Goodbye, rope!" (rope:write-rope inserted nil)))
    (is (string= "Hello, super rope!" (rope:write-rope super nil)))))

(deftest index-rope ()
  "Test accessing characters by index"
  (let* ((rope (rope:make-rope "0123456789")))
    (is (string= #\1 (rope:index-rope rope 1)))
    (is (string= #\9 (rope:index-rope rope 9)))))
