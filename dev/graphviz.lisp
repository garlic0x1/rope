(in-package #:rope/dev)

(defparameter *string* "In computer programming, a rope, or cord, is a data structure composed of smaller strings that is used to efficiently store and manipulate longer strings or entire texts.")

(defparameter rope::*long-leaf* 24)
(defparameter rope::*short-leaf* 8)

(defclass root ()
  ((rope :initarg :rope :accessor root-rope)))

(defmethod graph-object-node ((self (eql 'rope)) (obj root))
  (let ((obj (root-rope obj)))
    (make-instance 'node
                   :attributes `(:label ,(format nil "length: ~a~%depth: ~a"
                                                 (rope-length obj)
                                                 (rope-depth obj))
                                 :style :filled
                                 ))))

(defmethod graph-object-points-to ((self (eql 'rope)) (obj root))
  (let ((obj (root-rope obj)))
    (graph-object-points-to self obj)))

(defmethod graph-object-node ((self (eql 'rope)) (obj branch))
  (make-instance 'node
                 :attributes `(:label ,(format nil "length: ~a~%depth: ~a"
                                               (rope-length obj)
                                               (rope-depth obj)))))

(defmethod graph-object-points-to ((self (eql 'rope)) (obj branch))
  (list (make-instance 'attributed
                       :object (branch-right obj)
                       :attributes `(:label "R"))
        (make-instance 'attributed
                       :object (branch-left obj)
                       :attributes `(:label "L"))))

(defmethod graph-object-node ((self (eql 'rope)) (obj leaf))
  (make-instance 'node
                 :attributes `(:label ,(rope::leaf-string obj)
                               :shape :box)))

(defun graph-ropes (ropes &key (output-file "/tmp/graph.png"))
  (cl-dot:dot-graph (cl-dot:generate-graph-from-roots
                     'rope
                     (mapcar (lambda (rope)
                               (make-instance 'root :rope rope))
                             ropes))
                    output-file
                    :format :png)
  #+slynk (slynk:ed-in-emacs output-file))

#+example
(graph-ropes (list (rope::make-rope *string*)))

#+example
(let ((rope (rope::make-rope *string*)))
  (multiple-value-bind (ante post) (rope::split-rope rope 30)
    (graph-ropes (list rope ante post))))

#+example
(let ((rope (rope::make-rope "Hello")))
  (setf rope (append-rope rope ", "))
  (print (rope::balancedp rope))
  (setf rope (append-rope rope "world!"))
  (print (rope::balancedp rope))
  (setf rope (append-rope rope " it's nice to meet you!"))
  (print (rope::balancedp rope))
  ;; (setf rope (append-rope rope " hopefully"))
  ;; (print (rope::balancedp rope))
  ;; (setf rope (append-rope rope " we can do it again"))
  ;; (print (rope::balancedp rope))
  ;; (setf rope (append-rope rope " later!"))
  (graph-ropes (list rope)))
