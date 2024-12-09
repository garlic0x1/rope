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
    :accessor branch-right))
  (:documentation "A node with left and right children."))

(defclass leaf (rope)
  ((string
    :initarg :string
    :initform ""
    :accessor leaf-string))
  (:documentation "A string segment of a rope."))

;;-------;;
;; Utils ;;
;;-------;;

(defun branch-weight (branch)
  (rope-length (branch-left branch)))

(defun leaf-short-p (leaf)
  (>= *short-leaf* (rope-length leaf)))

(defun strcat (a b)
  (concatenate 'string a b))

(defun make-leaf (string &optional length)
  (make-instance 'leaf :string string :length (or length (length string))))

(defgeneric make-rope (source)
  (:documentation "Create a new rope from a string, stream, or pathname.")
  (:method ((source rope))
    source)
  (:method ((source stream))
    (labels ((read-leaves (&optional acc)
               (let* ((string (make-string *long-leaf*))
                      (length (read-sequence string source))
                      (leaf (make-leaf (subseq string 0 length) length)))
                 (if (= *long-leaf* length)
                     (read-leaves (cons leaf acc))
                     (cons leaf acc)))))
      (let ((leaves (nreverse (read-leaves))))
        (merge-leaves leaves 0 (length leaves)))))
  (:method ((source pathname))
    (with-open-file (s source)
      (make-rope s)))
  (:method ((source string))
    (let ((length (length source)))
      (if (<= *long-leaf* length)
          (concat-rope (make-rope (subseq source 0 (round length 2)))
                       (make-rope (subseq source (round length 2))))
          (make-leaf source length)))))

;;-----------;;
;; Iteration ;;
;;-----------;;

(defgeneric walk-rope (rope func)
  (:documentation "Call `func' on each leaf of a rope in order.")
  (:method ((rope leaf) func)
    (funcall func rope)
    (values))
  (:method ((rope branch) func)
    (walk-rope (branch-left rope) func)
    (walk-rope (branch-right rope) func)))

(defun write-rope (rope out)
  "Write a rope to a stream or string, like `format', nil output returns a string."
  (if (null out)
      (with-output-to-string (s) (write-rope rope s))
      (walk-rope rope
                 (lambda (leaf)
                   (write-string (leaf-string leaf) out)))))

(defun collect-rope (rope)
  (let (leaves)
    (walk-rope rope (lambda (leaf) (push leaf leaves)))
    (nreverse leaves)))

;;-----------;;
;; Balancing ;;
;;-----------;;

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

(defgeneric balancedp (rope)
  (:documentation "Check if a rope is a height-balanced tree.")
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
  "Balance a rope by reconstructing it from the bottom up."
  (if (and (balancedp rope) (not forcep))
      rope
      (let ((leaves (normalize-leaves (collect-rope rope))))
        (merge-leaves leaves 0 (length leaves)))))

;;--------;;
;; Insert ;;
;;--------;;

(defgeneric prepend-rope (rope source)
  (:documentation "Return a new rope with a string or rope inserted at the beginning of a rope.")
  (:method (rope (source string))
    (concat-rope (make-rope source) rope))
  (:method (rope (source rope))
    (concat-rope source rope)))

(defgeneric append-rope (rope source)
  (:documentation "Return a new rope with a string or rope inserted at the end of a rope.")
  (:method (rope (source string))
    (concat-rope rope (make-rope source)))
  (:method (rope (source rope))
    (concat-rope rope source)))

(defun insert-rope (rope index str)
  "Return a new rope with a string or rope inserted at the specified index of a rope."
  (cond ((= index 0) (prepend-rope rope str))
        ((= index (rope-length rope)) (append-rope rope str))
        (t (multiple-value-bind (ante post) (split-rope rope index)
             (concat-rope (append-rope ante (make-rope str)) post)))))

;;-------;;
;; Index ;;
;;-------;;

(defgeneric index-rope (rope index)
  (:documentation "Get a character at the specified index of a rope.")
  (:method ((rope leaf) index)
    (char (leaf-string rope) index))
  (:method ((rope branch) index)
    (let ((weight (branch-weight rope)))
      (if (< index weight)
          (index-rope (branch-left rope) index)
          (index-rope (branch-right rope) (- index weight))))))

(defun substr-rope (rope from &optional to)
  "Get a substring out of a rope."
  (multiple-value-bind (ante _) (split-rope rope (or to (rope-length rope)))
    (declare (ignore _))
    (multiple-value-bind (_ post) (split-rope ante from)
      (declare (ignore _))
      (write-rope post nil))))

;;--------;;
;; Concat ;;
;;--------;;

(defun concat-rope (left right)
  "Returns a balanced concatenation of two ropes."
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
  (:documentation "Return balanced ropes split at index as multiple values.")
  (:method ((rope leaf) index)
    (values (make-rope (subseq (leaf-string rope) 0 index))
            (make-rope (subseq (leaf-string rope) index))))
  (:method ((rope branch) index)
    (with-slots (left right) rope
      (let ((weight (branch-weight rope)))
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
  "Return a new rope without the characters in the specified range."
  (multiple-value-bind (ante _) (split-rope rope from)
    (declare (ignore _))
    (multiple-value-bind (_ post) (split-rope rope (or to from))
      (declare (ignore _))
      (concat-rope ante post))))
