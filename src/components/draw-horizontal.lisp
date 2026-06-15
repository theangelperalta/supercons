;;;; components/draw-horizontal.lisp
;;;;
;;;; Port of components/draw_horizontal.rs. A stateful builder (NOT a component)
;;;; that draws components side by side, each receiving the remaining width.

(in-package #:supercons)

(defclass draw-horizontal ()
  ((dim :initarg :dim :accessor draw-horizontal-dim)
   (rem-width :initarg :rem-width :accessor draw-horizontal-rem-width)
   (blocks :initarg :blocks :accessor draw-horizontal-blocks))
  (:documentation "Builder that stacks component output horizontally."))

(defun make-draw-horizontal (dimensions)
  "Create a horizontal draw builder bounded by DIMENSIONS."
  (make-instance 'draw-horizontal
                 :dim dimensions
                 :rem-width (dimensions-width dimensions)
                 :blocks '()))

(defun draw-horizontal-draw (builder component mode)
  "Draw COMPONENT into BUILDER using the remaining horizontal space."
  (let ((output (draw component
                      (make-dimensions (draw-horizontal-rem-width builder)
                                       (dimensions-height (draw-horizontal-dim builder)))
                      mode)))
    (setf (draw-horizontal-rem-width builder)
          (max 0 (- (draw-horizontal-rem-width builder)
                    (lines-max-line-length output))))
    (setf (draw-horizontal-blocks builder)
          (append (draw-horizontal-blocks builder) (list output))))
  (values))

(defun draw-horizontal-finish (builder)
  "Finish BUILDER, joining the blocks horizontally and shrinking to dimensions."
  (let ((lines (lines-join-horizontally (draw-horizontal-blocks builder))))
    (lines-shrink-lines-to-dimensions lines (draw-horizontal-dim builder))
    lines))
