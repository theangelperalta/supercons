;;;; components/draw-vertical.lisp
;;;;
;;;; Port of components/draw_vertical.rs. A stateful builder (NOT a component)
;;;; that draws components vertically, one after another, each receiving the
;;;; remaining vertical space.

(in-package #:supercons)

(defclass draw-vertical ()
  ((dim :initarg :dim :accessor draw-vertical-dim)
   (lines :initarg :lines :accessor draw-vertical-lines))
  (:documentation "Builder that stacks component output vertically."))

(defun make-draw-vertical (dimensions)
  "Create a vertical draw builder bounded by DIMENSIONS."
  (make-instance 'draw-vertical :dim dimensions :lines (make-lines)))

(defun draw-vertical-draw (builder component mode)
  "Draw COMPONENT into BUILDER using the remaining vertical space."
  (let* ((dim (draw-vertical-dim builder))
         (used (lines-len (draw-vertical-lines builder)))
         (output (draw component
                       (make-dimensions (dimensions-width dim)
                                        (max 0 (- (dimensions-height dim) used)))
                       mode)))
    (lines-extend (draw-vertical-lines builder) (lines-vec output)))
  (values))

(defun draw-vertical-finish (builder)
  "Finish BUILDER, returning the accumulated lines shrunk to its dimensions."
  (lines-shrink-lines-to-dimensions (draw-vertical-lines builder)
                                    (draw-vertical-dim builder))
  (draw-vertical-lines builder))
