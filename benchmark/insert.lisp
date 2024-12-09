(in-package #:rope/benchmark)

;; (progn
;;   (sb-ext:restrict-compiler-policy 'speed 3 3)
;;   (sb-ext:restrict-compiler-policy 'debug 0 0)
;;   ;; (sb-ext:restrict-compiler-policy 'space 0 0)
;;   (sb-ext:restrict-compiler-policy 'safety 0 0))


(defparameter *readme*
  (merge-pathnames "README.md" (asdf:system-source-directory :rope)))

(defun print-time (time reps length)
  (format t "rope size: ~a, ~a microseconds~%"
          length
          (* 1000000 (/ time reps))))

(defmacro time* (&body body)
  `(let ((time))
     (sb-ext:call-with-timing
      (lambda (&rest plist) (setf time (getf plist :real-time-ms)))
      (lambda () ,@body))
     (coerce (/ time 1000) 'float)))

(defun benchmark-insert (&optional (reps 1000000))
  (with-open-file (s *readme*)
    (let* ((starting-rope (rope:split-rope (rope:make-rope s) 1000))
           (rope starting-rope))
      (dotimes (i 100)
        (print-time
         (time*
          (dotimes (i reps)
            (rope:insert-rope rope
                              (random (rope:rope-length rope))
                              "Hello, world!")))
         reps
         (rope:rope-length rope))
        (force-output)
        (setf rope (rope:concat-rope rope starting-rope))))))
