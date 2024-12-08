(fiasco:define-test-package :rope/test/fuzz
  (:local-nicknames (#:a #:alexandria)))
(in-package :rope/test/fuzz)

(defvar *charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defun random-string (&key (length 128) (acc ""))
  (if (= 0 length)
      acc
      (random-string
       :length (1- length)
       :acc (rope::strcat acc (string (a:random-elt *charset*))))))

(deftest fuzz-basic-tests ()
  "Run the basic test suite with different leaf sizes."
  (loop :for i :from 1 :to 64
        :do (setf rope::*short-leaf* i)
            (setf rope::*long-leaf* (* 4 i))
            (is (run-tests :rope/test/basic))))

(deftest fuzz-split ()
  (dotimes (i 10)
    (let* ((length (random 10000))
           (string (random-string :length length))
           (rope (rope:make-rope string))
           (index (random length)))
      (multiple-value-bind (ante post) (rope:split-rope rope index)
        (is (string= (subseq string 0 index) (rope:write-rope ante nil)))
        (is (string= (subseq string index) (rope:write-rope post nil)))))))

(deftest fuzz-index ()
  (dotimes (i 100)
    (let* ((length (random 1000))
           (string (random-string :length length))
           (rope (rope:make-rope string))
           (index (random length)))
      (is (char= (char string index) (rope:index-rope rope index))))))

(deftest fuzz-concat ()
  (dotimes (i 100)
    (let* ((string-a (random-string :length (random 1000)))
           (string-b (random-string :length (random 1000)))
           (rope-a (rope:make-rope string-a))
           (rope-b (rope:make-rope string-b)))
      (is (string= (rope::strcat string-a string-b)
                   (rope:write-rope (rope:concat-rope rope-a rope-b) nil))))))

(deftest fuzz-kill ()
  (dotimes (i 10000)
    (let* ((length (+ 10 (random 1000)))
           (string (random-string :length length))
           (rope (rope:make-rope string))
           (end (1+ (random (1- length))))
           (start (random end)))
      (is (string= (rope::strcat (subseq string 0 start)
                                 (subseq string end))
                   (rope:write-rope (rope:kill-rope rope start end) nil))))))
