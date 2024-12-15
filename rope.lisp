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

;; Balance: -2
;; Left Rotation
;;   a              c
;;  / \           /    \
;; b   c         a      e
;;    / \       / \    / \
;;   d   e     b   d  f   g
;;      / \
;;     f   g

;; Balance: 2
;; Right Rotation
;;       a          b
;;      / \       /    \
;;     b   c     d      a
;;    / \       / \    / \
;;   d   e     f   g  e   c
;;  / \
;; f   g

(defgeneric balance-factor (rope)
  (:method ((rope leaf))
    0)
  (:method ((rope branch))
    (- (rope-depth (branch-left rope))
       (rope-depth (branch-right rope)))))

(defun rotate-left (rope)
  (with-slots (left right) rope
    (concat-rope*
     (concat-rope left (branch-left right))
     (branch-right right))))

(defun rotate-right (rope)
  (with-slots (left right) rope
    (concat-rope*
     (branch-left left)
     (concat-rope (branch-right left) right))))

(defun rotate-left-right (rope)
  (with-slots (left right) rope
    (rotate-right (concat-rope* (rotate-left left) right))))

(defun rotate-right-left (rope)
  (with-slots (left right) rope
    (rotate-left (concat-rope* left (rotate-right right)))))

(defun balance-children (rope)
  (with-slots (left right) rope
    (concat-rope* (balance-rope left)
                  (balance-rope right))))

(defgeneric balance-rope (rope)
  (:method ((rope leaf))
    rope)
  (:method ((rope branch))
    (with-slots (left right) rope
      (let ((bf (balance-factor rope)))
        (cond ((< 0 bf)
               ;; (balance-children)
               (if (minusp (balance-factor left))
                   (rotate-left-right rope)
                   (rotate-right rope)))
              ((> 0 bf)
               ;; (balance-children)
               (if (plusp (balance-factor right))
                   (rotate-right-left rope)
                   (rotate-left rope)))
              (t
               rope))))
    ;; (with-slots (left right) rope
    ;;   (let ((left (if (zerop (balance-factor left))
    ;;                   left
    ;;                   (balance-rope left)))
    ;;         (right (if (zerop (balance-factor right))
    ;;                    right
    ;;                    (balance-rope right)))
    ;;         (rope (concat-rope* left right))
    ;;         (bf (balance-factor rope)))
    ;;     (format t "root: ~a, left: ~a, right: ~a~%"
    ;;             bf
    ;;             (balance-factor left)
    ;;             (balance-factor right)
    ;;             )
    ;;     (cond ((< 0 bf)
    ;;            (if (minusp (balance-factor left))
    ;;                (rotate-left-right rope)
    ;;                (rotate-right rope)))
    ;;           ((> 0 bf)
    ;;            (if (plusp (balance-factor right))
    ;;                (rotate-right-left rope)
    ;;                (rotate-left rope)))
    ;;           (t
    ;;            rope))))
    ;; (with-slots (left right) rope
    ;;   (let* ((left (if (balance-direction left) (balance-rope left) left))
    ;;          (right (if (balance-direction right) (balance-rope right) right))
    ;;          (rope (concat-rope* left right))
    ;;          (balance (balance rope)))
    ;;     (cond ((< 1 balance)  (rotate rope :right))
    ;;           ((> -1 balance) (rotate rope :left))
    ;;           (t              rope))))
    ))

;;---------;;
;; Rebuild ;;
;;---------;;

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

(defun merge-leaves (leaves start end)
  (let ((range (- end start)))
    (case range
      (1 (nth start leaves))
      (2 (concat-rope (nth start leaves) (nth (1+ start) leaves)))
      (t (let ((mid (+ start (round (/ range 2)))))
           (concat-rope (merge-leaves leaves start mid)
                        (merge-leaves leaves mid end)))))))

(defun rebuild-rope (rope)
  "Balance a rope by reconstructing it from the bottom up."
  (let ((leaves (normalize-leaves (collect-rope rope))))
    (merge-leaves leaves 0 (length leaves))))

;;--------;;
;; Insert ;;
;;--------;;

(defgeneric prepend-rope (rope source)
  (:documentation "Return a new rope with a string or rope inserted at the beginning of a rope.")
  (:method (rope (source rope))
    (concat-rope source rope))
  (:method (rope (source t))
    (concat-rope (make-rope source) rope)))

(defgeneric append-rope (rope source)
  (:documentation "Return a new rope with a string or rope inserted at the end of a rope.")
  (:method (rope (source rope))
    (concat-rope rope source))
  (:method (rope (source t))
    (concat-rope rope (make-rope source))))

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

(defun concat-rope* (left right)
  "Concatenates without balancing."
  (make-instance 'branch
                 :length (+ (rope-length left) (rope-length right))
                 :depth (1+ (max (rope-depth left) (rope-depth right)))
                 :left left
                 :right right))

(defun concat-rope (left right)
  "Returns a balanced concatenation of two ropes."
  (balance-rope (concat-rope* left right)))

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
                         (concat-rope post right))))
              ((> index weight)
               (multiple-value-bind (ante post) (split-rope right (- index weight))
                 (values (concat-rope left ante)
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
