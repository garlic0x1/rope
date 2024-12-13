(in-package #:rope/dev)

(defparameter *string* "In computer programming, a rope, or cord, is a data structure composed of smaller strings that is used to efficiently store and manipulate longer strings or entire texts.")

(defparameter rope::*long-leaf* 24)
(defparameter rope::*short-leaf* 2)

(defclass root ()
  ((name :initarg :name :accessor root-name)
   (rope :initarg :rope :accessor root-rope)))

(defmethod graph-object-node ((self (eql 'rope)) (root root))
  (let ((obj (root-rope root)))
    (make-instance 'node
                   :attributes `(:label ,(format nil "rope: ~a~%length: ~a~%depth: ~a"
                                                 (root-name root)
                                                 (rope-length obj)
                                                 (rope::rope-depth obj))
                                 :style :filled))))

(defmethod graph-object-points-to ((self (eql 'rope)) (obj root))
  (let ((obj (root-rope obj)))
    (graph-object-points-to self obj)))

(defmethod graph-object-node ((self (eql 'rope)) (obj rope::branch))
  (make-instance 'node
                 :attributes `(:label ,(format nil "length: ~a~%depth: ~a"
                                               (rope-length obj)
                                               (rope::rope-depth obj)))))

(defmethod graph-object-points-to ((self (eql 'rope)) (obj rope::branch))
  (list (make-instance 'attributed
                       :object (rope::branch-right obj)
                       :attributes `(:label "R"))
        (make-instance 'attributed
                       :object (rope::branch-left obj)
                       :attributes `(:label "L"))))

(defmethod graph-object-node ((self (eql 'rope)) (obj rope::leaf))
  (make-instance 'node
                 :attributes `(:label ,(format nil "~a"
                                               (rope::leaf-string obj))
                               :shape :box)))

(defun graph-ropes (ropes &key (output-file "/tmp/graph.png"))
  (dot-graph (generate-graph-from-roots
              'rope
              (mapcar (lambda (rope)
                        (if (consp rope)
                            (make-instance 'root
                                           :name (car rope)
                                           :rope (cdr rope))
                            (make-instance 'root
                                           :name :root-node
                                           :rope rope)))
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
  (setf rope (append-rope rope "world!"))
  (setf rope (append-rope rope " it's nice to meet you!"))
  (setf rope (append-rope rope " hopefully"))
  (setf rope (append-rope rope " we can do it again"))
  (setf rope (append-rope rope " later!"))
  (graph-ropes (list rope)))

#+example
(let ((rope (rope::make-rope "hello world")))
  (dotimes (i 10)
    (setf rope (rope:append-rope rope "!")))
  (graph-ropes (list (rope::balance-rope rope)
                     )))
