(in-package #:rope/dev)

(defparameter *string* "In computer programming, a rope, or cord, is a data structure composed of smaller strings that is used to efficiently store and manipulate longer strings or entire texts.")

(defparameter rope::*long-leaf* 24)
(defparameter rope::*short-leaf* 8)

(defmethod graph-object-node ((self (eql 'rope)) (obj branch))
  (make-instance 'node
                 :attributes `(:label ,(format nil "length: ~a~%depth: ~a"
                                               (rope-length obj)
                                               (rope-depth obj)))))

(defmethod graph-object-points-to ((self (eql 'rope)) (obj branch))
  (list (branch-right obj) (branch-left obj)))

(defmethod graph-object-node ((self (eql 'rope)) (obj leaf))
  (make-instance 'node
                 :attributes `(:label ,(rope::leaf-string obj)
                               :shape :box)))

(defun graph-rope (rope &key (output-file "/tmp/graph.png"))
  (cl-dot:dot-graph (cl-dot:generate-graph-from-roots
                     'rope
                     (list rope))
                    output-file
                    :format :png)
  #+slynk (slynk:ed-in-emacs output-file))

#+example
(graph-rope (rope::make-rope *string*))
