;;;; components/blank.lisp
;;;;
;;;; Port of components/blank.rs. `blank` is a dead-end component that emits
;;;; nothing; it is the default component of a fresh SuperConsole.

(in-package #:supercons)

(defclass blank (component) ()
  (:documentation "A component that emits no lines."))

(defun make-blank ()
  "Construct a `blank` component."
  (make-instance 'blank))

(defmethod draw-unchecked ((component blank) dimensions mode)
  (declare (ignore dimensions mode))
  (make-lines))
