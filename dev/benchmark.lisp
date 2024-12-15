(in-package #:rope/dev)

;; (progn
;;   (sb-ext:restrict-compiler-policy 'speed 3 3)
;;   (sb-ext:restrict-compiler-policy 'debug 0 0)
;;   ;; (sb-ext:restrict-compiler-policy 'space 0 0)
;;   (sb-ext:restrict-compiler-policy 'safety 0 0))

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

(defun print-time (time reps length)
  (format t "rope size: ~a, ~a microseconds~%"
          length
          (* 1000000 (/ time reps)))
  (force-output))

(defmacro time* (&body body)
  `(let ((time))
     (sb-ext:call-with-timing
      (lambda (&rest plist) (setf time (getf plist :real-time-ms)))
      (lambda () ,@body))
     (coerce (/ time 1000) 'float)))

(defun benchmark-size (size step times reps)
  (let ((rope (random-rope size))
        (step (random-rope step)))
    (dotimes (i times)
      (sb-ext:gc :full t)
      (print-time
       (time*
         (dotimes (i reps)
           (insert-rope rope (random (rope-length rope)) "Hello, world!")))
       reps
       (rope-length rope))
      (setf rope (concat-rope rope step)))))

(defun benchmark-log (start-size limit reps)
  (let ((rope (random-rope start-size))
        (step (random-rope start-size)))
    (dotimes (i limit)
      (dotimes (i 10)
        (unless (= i 0)
          (setf rope (insert-rope rope
                                  (random (rope-length rope))
                                  step)))
        (sb-ext:gc :full t)
        (print-time
         (time*
           (dotimes (i reps)
             (insert-rope rope (random (rope-length rope)) "Hello, world!")))
         reps
         (rope-length rope)))
      (setf step rope))))

(defun benchmark-insert (&optional (reps 1000000))
  (setf rope::*long-leaf* 512)
  (benchmark-log 1000 6 reps))
