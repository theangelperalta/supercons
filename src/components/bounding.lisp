;;;; components/bounding.lisp
;;;;
;;;; Port of components/bounding.rs. `bounded` constrains its child component to
;;;; at most `max-size` render space.

(in-package #:supercons)

(defclass bounded (component)
  ((child :initarg :child :accessor bounded-child)
   (max-size :initarg :max-size :accessor bounded-max-size))
  (:documentation "Constrains CHILD to at most MAX-SIZE dimensions."))

(defun make-bounded (child &key max-x max-y)
  "Construct a `bounded` wrapping CHILD. NIL bounds are treated as unbounded."
  (make-instance 'bounded
                 :child child
                 :max-size (make-dimensions (or max-x most-positive-fixnum)
                                            (or max-y most-positive-fixnum))))

(defmethod draw-unchecked ((component bounded) dimensions mode)
  (draw (bounded-child component)
        (dimensions-intersect dimensions (bounded-max-size component))
        mode))
