(in-package #:rope)

(defparameter *short-leaf* 16)
(defparameter *long-leaf* 128)

(defclass rope ()
  ((length
    :initarg :length
    :initform 0
    :accessor rope-length)
   (depth
    :initarg :depth
    :initform 0
    :accessor rope-depth)))

(defclass branch (rope)
  ((left
    :initarg :left
    :initform nil
    :accessor branch-left)
   (right
    :initarg :right
    :initform nil
    :accessor branch-right)))

(defclass leaf (rope)
  ((string
    :initarg :string
    :initform ""
    :accessor leaf-string)))

;;-------;;
;; Utils ;;
;;-------;;

(defun fibonacci (n)
  (case n
    (0 0)
    (1 1)
    (t (+ (fibonacci (- n 1))
          (fibonacci (- n 2))))))

(defgeneric make-rope (source)
  (:method ((source string))
    (let ((length (length source)))
      (if (<= *long-leaf* length)
          (concat-rope (make-rope (subseq source 0 (round length 2)))
                       (make-rope (subseq source (round length 2))))
          (make-instance 'leaf :length length :string source)))))

(defgeneric rope-weight (rope)
  (:method ((rope leaf))
    (rope-length rope))
  (:method ((rope branch))
    (rope-length (branch-left rope))))

;;-----------;;
;; Iteration ;;
;;-----------;;

(defgeneric walk-rope (rope func)
  (:method ((rope leaf) func)
    (funcall func rope))
  (:method ((rope branch) func)
    (walk-rope (branch-left rope) func)
    (walk-rope (branch-right rope) func)))

(defun write-rope (rope out)
  (if (null out)
      (with-output-to-string (s) (write-rope rope s))
      (walk-rope rope
                 (lambda (leaf)
                   (write-string (leaf-string leaf) out)))))

(defun collect-rope (rope)
  (let ((leaves '()) (length 0))
    (walk-rope rope (lambda (leaf) (push leaf leaves) (incf length)))
    (values (nreverse leaves) length)))

;;-----------;;
;; Balancing ;;
;;-----------;;

(defgeneric balancedp (rope)
  (:method ((rope leaf))
    t)
  (:method ((rope branch))
    (<= (fibonacci (+ 2 (rope-depth rope)))
        (rope-length rope))))

(defun merge-leaves (leaves start end)
  (let ((range (- end start)))
    (case range
      (1 (first leaves))
      (2 (concat-rope (first leaves) (second leaves)))
      (t (let ((mid (+ start (/ range 2))))
           (concat-rope (merge-leaves leaves start mid)
                        (merge-leaves leaves mid end)))))))

(defun balance-rope (rope)
  (if (balancedp rope)
      rope
      (multiple-value-bind (leaves length) (collect-rope rope)
        (merge-leaves leaves 0 length))))

;;--------;;
;; Insert ;;
;;--------;;

(defun prepend-rope (rope str)
  (concat-rope (make-rope str) rope))

(defun append-rope (rope str)
  (concat-rope rope (make-rope str)))

(defun insert-rope (rope index str)
  (cond ((= index 0) (prepend-rope rope str))
        ((= index (rope-length rope)) (append-rope rope str))
        (t (multiple-value-bind (ante post) (split-rope rope index)
             (concat-rope (append-rope ante (make-rope str)) post)))))

;;-------;;
;; Index ;;
;;-------;;

(defgeneric index-rope (rope index)
  (:method ((rope leaf) index)
    (char (leaf-string rope) index))
  (:method ((rope branch) index)
    (let ((weight (rope-weight rope)))
      (if (< index weight)
          (index-rope (branch-left rope) index)
          (index-rope (branch-right rope) (- index weight))))))

;;--------;;
;; Concat ;;
;;--------;;

(defmethod concat-rope (left right)
  (make-instance 'branch
                 :length (+ (rope-length left) (rope-length right))
                 :depth (1+ (max (rope-depth left) (rope-depth right)))
                 :left left
                 :right right))

;;-------;;
;; Split ;;
;;-------;;

(defgeneric split-rope (rope index)
  (:method ((rope leaf) index)
    (values (make-rope (subseq (leaf-string rope) 0 index))
            (make-rope (subseq (leaf-string rope) index))))
  (:method ((rope branch) index)
    (with-slots (left right) rope
      (let ((weight (rope-weight rope)))
        (cond ((= index weight)
               (values left right))
              ((< index weight)
               (multiple-value-bind (ante post) (split-rope left index)
                 (values ante (concat-rope post right))))
              ((> index weight)
               (multiple-value-bind (ante post) (split-rope right (- index weight))
                 (values (concat-rope left ante) post))))))))

;;--------;;
;; Delete ;;
;;--------;;
