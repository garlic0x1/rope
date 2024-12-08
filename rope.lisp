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

(defun leaf-short-p (leaf)
  (>= *short-leaf* (rope-length leaf)))

(defun strcat (a b)
  (concatenate 'string a b))

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
  (let (leaves)
    (walk-rope rope (lambda (leaf) (push leaf leaves)))
    (nreverse leaves)))

(defun normalize-leaves (leaves &optional carry)
  (let ((leaf (car leaves)))
    (cond ((and carry (null leaf))
           (list (make-rope carry)))
          ((null leaf)
           nil)
          (carry
           (append (collect-rope (make-rope (strcat carry (leaf-string leaf))))
                   (normalize-leaves (cdr leaves))))
          ((leaf-short-p leaf)
           (normalize-leaves (cdr leaves) (leaf-string leaf)))
          (t
           (cons leaf (normalize-leaves (cdr leaves)))))))

;;-----------;;
;; Balancing ;;
;;-----------;;

(defgeneric balancedp (rope)
  (:method ((rope leaf))
    t)
  (:method ((rope branch))
    (with-slots (left right) rope
      (and (balancedp left)
           (balancedp right)
           (>= 2 (abs (- (rope-depth left) (rope-depth right))))))))

(defun merge-leaves (leaves start end)
  (let ((range (- end start)))
    (case range
      (1 (nth start leaves))
      (2 (concat-rope (nth start leaves) (nth (1+ start) leaves)))
      (t (let ((mid (+ start (round (/ range 2)))))
           (concat-rope (merge-leaves leaves start mid)
                        (merge-leaves leaves mid end)))))))

(defun balance-rope (rope &optional forcep)
  (if (and (balancedp rope) (not forcep))
      rope
      (let ((leaves (normalize-leaves (collect-rope rope))))
        (merge-leaves leaves 0 (length leaves)))))

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
  (balance-rope
   (make-instance 'branch
                  :length (+ (rope-length left) (rope-length right))
                  :depth (1+ (max (rope-depth left) (rope-depth right)))
                  :left left
                  :right right)))

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
                 (values (balance-rope ante)
                         (balance-rope (concat-rope post right)))))
              ((> index weight)
               (multiple-value-bind (ante post) (split-rope right (- index weight))
                 (values (balance-rope (concat-rope left ante))
                         (balance-rope post)))))))))

;;------;;
;; Kill ;;
;;------;;

(defun kill-rope (rope from &optional to)
  (multiple-value-bind (ante _) (split-rope rope from)
    (declare (ignore _))
    (multiple-value-bind (_ post) (split-rope rope (or to from))
      (declare (ignore _))
      (concat-rope ante post))))
