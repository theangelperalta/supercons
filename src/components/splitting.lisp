;;;; components/splitting.lisp
;;;;
;;;; Port of components/splitting.rs. `split` divides space along a direction and
;;;; draws its children into the resulting regions.
;;;;
;;;; A split kind is one of:
;;;;   :equal                 -- equal share per child
;;;;   :adaptive              -- each child takes as much as it wants
;;;;   (list :sized RATIOS)   -- shares proportional to RATIOS (one per child)

(in-package #:supercons)

(defun to-internal-split-kind (kind children-len)
  "Normalize a public split KIND into an internal one: either :adaptive or
(list :sized-normalized NORMALIZED-RATIOS)."
  (cond
    ((eq kind :adaptive) :adaptive)
    ((eq kind :equal)
     (list :sized-normalized
           (if (zerop children-len)
               '()
               (make-list children-len :initial-element (/ 1.0d0 children-len)))))
    ((and (consp kind) (eq (car kind) :sized))
     (let ((sizes (cadr kind)))
       (assert (= (length sizes) children-len) ()
               "There must be an equal number of ratios and children.")
       (let ((total (reduce #'+ sizes)))
         (list :sized-normalized (mapcar (lambda (s) (/ s total)) sizes)))))
    (t (error "Invalid split kind: ~a" kind))))

(defclass split (component)
  ((children :initarg :children :accessor split-children)
   (direction :initarg :direction :accessor split-direction)
   (kind :initarg :kind :accessor split-kind))
  (:documentation "Splits space along DIRECTION among CHILDREN per KIND."))

(defun make-split (children direction kind)
  "Construct a `split`. KIND is :equal, :adaptive, or (list :sized RATIOS)."
  (make-instance 'split
                 :children children
                 :direction direction
                 :kind (to-internal-split-kind kind (length children))))

(defun %split-draw (kind children direction dimensions mode)
  "Draw each child into its allotted region, returning a list of `lines`."
  (if (eq kind :adaptive)
      (let ((available dimensions) (outputs '()))
        (dolist (child children (nreverse outputs))
          (let ((output (draw child available mode)))
            (lines-shrink-lines-to-dimensions output dimensions)
            (let ((used (dimension-from-output-truncated output direction)))
              (setf available (dimensions-saturating-sub available used direction)))
            (push output outputs))))
      (let ((sizes (cadr kind)) (outputs '()))
        (loop for child in children
              for size in sizes
              do (let* ((child-dim (dimensions-multiply dimensions size direction))
                        (output (draw child child-dim mode)))
                   (ecase direction
                     (:horizontal
                      (lines-truncate-lines-bottom output (dimensions-height child-dim))
                      (lines-set-lines-to-exact-width output (dimensions-width child-dim)))
                     (:vertical
                      (lines-truncate-lines output (dimensions-width child-dim))
                      (lines-set-lines-to-exact-length output (dimensions-height child-dim))))
                   (push output outputs)))
        (nreverse outputs))))

(defmethod draw-unchecked ((component split) dimensions mode)
  (let ((outputs (%split-draw (split-kind component) (split-children component)
                              (split-direction component) dimensions mode)))
    (ecase (split-direction component)
      (:horizontal (lines-join-horizontally outputs))
      (:vertical (let ((all (make-lines)))
                   (dolist (output outputs all)
                     (lines-extend all (lines-vec output))))))))
