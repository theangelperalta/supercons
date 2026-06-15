;;;; components/padding.lisp
;;;;
;;;; Port of components/padding.rs. `padded` pads its child on all four sides,
;;;; truncating content preferentially over padding.

(in-package #:supercons)

(defclass padded (component)
  ((child :initarg :child :accessor padded-child)
   (left :initarg :left :accessor padded-left)
   (right :initarg :right :accessor padded-right)
   (top :initarg :top :accessor padded-top)
   (bottom :initarg :bottom :accessor padded-bottom))
  (:documentation "Pads CHILD by LEFT/RIGHT/TOP/BOTTOM columns/rows."))

(defun make-padded (child &key (left 0) (right 0) (top 0) (bottom 0))
  "Construct a `padded` wrapping CHILD."
  (make-instance 'padded :child child :left left :right right :top top :bottom bottom))

(defmethod draw-unchecked ((component padded) dimensions mode)
  (let ((output (draw (padded-child component) dimensions mode)))
    ;; Ordering matters: top/bottom rows must be padded horizontally too.
    (lines-pad-lines-top output (padded-top component))
    (lines-truncate-lines-bottom output (max 0 (- (dimensions-height dimensions)
                                                  (padded-bottom component))))
    (lines-pad-lines-bottom output (padded-bottom component))
    (lines-pad-lines-left output (padded-left component))
    (lines-truncate-lines output (max 0 (- (dimensions-width dimensions)
                                           (padded-right component))))
    (lines-pad-lines-right output (padded-right component))
    output))
