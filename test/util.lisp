(defpackage :rope/test/util
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria))
  (:export
   #:balancedp
   #:random-string
   #:random-rope
   #:random-path
   #:random-insert
   #:random-kill))
(in-package :rope/test/util)

(defgeneric balancedp (rope)
  (:method ((rope rope::leaf))
    t)
  (:method ((rope rope::branch))
    (and (>= 1 (abs (rope::balance-factor rope)))
         (balancedp (rope::branch-left rope))
         (balancedp (rope::branch-right rope)))))

(defvar *charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defun random-string (&key (length 128) (acc ""))
  (if (= 0 length)
      acc
      (random-string
       :length (1- length)
       :acc (rope::strcat acc (string (a:random-elt *charset*))))))

(defun random-rope (total-length)
  (with-open-file (source "/dev/urandom" :element-type '(unsigned-byte 8))
    (labels ((read-leaves (&optional acc (acc-length 0))
               (let* ((string* (make-array (min rope::*long-leaf* (- total-length acc-length))
                                           :element-type '(unsigned-byte 8)))
                      (length (read-sequence string* source))
                      (string (map 'string #'code-char string*))
                      (leaf (rope::make-leaf (subseq string 0 length) length)))
                 (if (and (= rope::*long-leaf* length)
                          (not (= total-length (+ length acc-length))))
                     (read-leaves (cons leaf acc) (+ length acc-length))
                     (cons leaf acc)))))
      (let ((leaves (nreverse (read-leaves))))
        (rope::merge-leaves leaves 0 (length leaves))))))

(defmacro random-path (&body forms)
  `(case (random ,(length forms))
     ,@(loop :with i := 0
             :for form :in forms
             :collect (list i form)
             :do (incf i))))

(defun random-insert (rope)
  (random-path
    (rope:insert-rope
     rope
     (random (rope:rope-length rope))
     (random-path
       (random-string :length (random 1024))
       (random-rope (random 1024))))
    (rope:append-rope
     rope
     (random-path
       (random-string :length (random 1024))
       (random-rope (random 1024))))
    (rope:prepend-rope
     rope
     (random-path
       (random-string :length (random 1024))
       (random-rope (random 1024))))))

(defun random-kill (rope)
  (random-path
    (rope:kill-rope rope (random (rope:rope-length rope)))
    ))
