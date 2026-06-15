;;;; components/echo.lisp
;;;;
;;;; Port of components/echo.rs. `echo` repeats whatever lines are put into it.
;;;; In Rust this is a test-only component; we expose it for the test suite.

(in-package #:supercons)

(defclass echo (component)
  ((lines :initarg :lines :accessor echo-lines))
  (:documentation "A component that echoes a fixed set of lines."))

(defun make-echo (lines)
  "Construct an `echo` component wrapping LINES."
  (make-instance 'echo :lines lines))

(defmethod draw-unchecked ((component echo) dimensions mode)
  (declare (ignore dimensions mode))
  (copy-lines (echo-lines component)))
