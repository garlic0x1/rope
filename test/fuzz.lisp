(fiasco:define-test-package :rope/test/fuzz
  (:use #:rope/test/util))
(in-package :rope/test/fuzz)

(deftest fuzz-basic-tests ()
  "Run the basic test suite with different leaf sizes."
  (loop :for i :from 1 :to 64
        :do (setf rope::*short-leaf* i)
            (setf rope::*long-leaf* (* 4 i))
            (let ((*standard-output* (make-broadcast-stream)))
              (is (run-tests :rope/test/basic)))))

(deftest fuzz-split ()
  (dotimes (i 1000)
    (let* ((length (random 10000))
           (string (random-string :length length))
           (rope (rope:make-rope string))
           (index (random length)))
      (multiple-value-bind (ante post) (rope:split-rope rope index)
        (is (balancedp ante))
        (is (balancedp post))
        (is (string= (subseq string 0 index) (rope:write-rope ante nil)))
        (is (string= (subseq string index) (rope:write-rope post nil)))))))

(deftest fuzz-index ()
  (dotimes (i 1000)
    (let* ((length (1+ (random 1000)))
           (string (random-string :length length))
           (rope (rope:make-rope string))
           (index (random length)))
      (is (char= (char string index) (rope:index-rope rope index))))))

(deftest fuzz-concat ()
  (dotimes (i 1000)
    (let* ((string-a (random-string :length (random 1000)))
           (string-b (random-string :length (random 1000)))
           (rope-a (rope:make-rope string-a))
           (rope-b (rope:make-rope string-b))
           (new-string (rope::strcat string-a string-b))
           (new-rope (rope:concat-rope rope-a rope-b)))
      (is (balancedp new-rope))
      (is (string= new-string (rope:write-rope new-rope nil))))))

(deftest fuzz-kill ()
  (dotimes (i 1000)
    (let* ((length (+ 10 (random 1000)))
           (string (random-string :length length))
           (rope (rope:make-rope string))
           (end (1+ (random (1- length))))
           (start (random end))
           (new-string (rope::strcat (subseq string 0 start) (subseq string end)))
           (new-rope (rope:kill-rope rope start end)))
      (is (balancedp new-rope))
      (is (string= new-string (rope:write-rope new-rope nil))))))

(deftest fuzz-insert-balance ()
  (setf rope::*long-leaf* 128)
  (dotimes (i 10)
    (let ((rope (random-rope 10)))
      (dotimes (i 1000)
        (setf rope
              (rope:insert-rope rope
                                (random (rope:rope-length rope))
                                (random-string :length (random 5))))
        (is (balancedp rope))))))

;; (deftest fuzz-random-actions-balance
;;     (&key (rope (random-rope (random 256)))
;;           (iterations 1000))
;;   (cond ((> 50000 (rope:rope-length rope))
;;          (fuzz-random-actions-balance
;;           :rope (random-insert rope)
;;           :iterations (1- iterations)))
;;         ((< 12 (rope:rope-length rope))
;;          (fuzz-random-actions-balance
;;           :rope (random-kill rope)
;;           :iterations (1- iterations)))))
