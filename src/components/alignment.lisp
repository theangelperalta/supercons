;;;; components/alignment.lisp
;;;;
;;;; Port of components/alignment.rs. `aligned` positions its child within the
;;;; bounding box. Horizontal alignment is one of :left, :left-justified,
;;;; :center, :right; vertical alignment is one of :top, :center, :bottom.

(in-package #:supercons)

(deftype horizontal-alignment-kind ()
  '(member :left :left-justified :center :right))

(deftype vertical-alignment-kind ()
  '(member :top :center :bottom))

(defclass aligned (component)
  ((child :initarg :child :accessor aligned-child)
   (horizontal :initarg :horizontal :accessor aligned-horizontal)
   (vertical :initarg :vertical :accessor aligned-vertical))
  (:documentation "Positions CHILD within the bounding box per HORIZONTAL/VERTICAL."))

(defun make-aligned (child &key (horizontal :left) (vertical :top))
  "Construct an `aligned` wrapping CHILD."
  (make-instance 'aligned :child child :horizontal horizontal :vertical vertical))

(defmethod draw-unchecked ((component aligned) dimensions mode)
  (let* ((width (dimensions-width dimensions))
         (height (dimensions-height dimensions))
         (output (draw (aligned-child component) dimensions mode))
         (pad-needed (max 0 (- height (lines-len output)))))
    (ecase (aligned-vertical component)
      (:top)
      (:center (let ((top-pad (floor pad-needed 2)))
                 (lines-pad-lines-top output top-pad)
                 (lines-pad-lines-bottom output (- pad-needed top-pad))))
      (:bottom (lines-pad-lines-top output pad-needed)))
    (ecase (aligned-horizontal component)
      (:left)
      (:left-justified (lines-justify output))
      (:center (dolist (line (lines-vec output))
                 (let* ((pn (max 0 (- width (line-len line))))
                        (left-pad (floor pn 2)))
                   (line-pad-left line left-pad)
                   (line-pad-right line (- pn left-pad)))))
      (:right (dolist (line (lines-vec output))
                (line-pad-left line (max 0 (- width (line-len line)))))))
    output))
