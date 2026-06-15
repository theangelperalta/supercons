;;;; component.lisp
;;;;
;;;; Port of components.rs: the `DrawMode` enum and the `Component` protocol.
;;;;
;;;; Rust models `Component` as a trait with an associated `Error` type and two
;;;; methods, `draw_unchecked` and `draw` (the latter defaulting to calling the
;;;; former and shrinking the result to the given dimensions). In Common Lisp we
;;;; use a base class plus two generic functions; the default `draw` method lives
;;;; on the base class so all components inherit the shrink behavior. Errors are
;;;; signalled as conditions rather than returned, so there is no associated error
;;;; type.

(in-package #:supercons)

;;; Draw mode -----------------------------------------------------------------

(defconstant +draw-mode-normal+ :normal)
(defconstant +draw-mode-final+ :final)

(deftype draw-mode () '(member :normal :final))

;;; Lines copying -------------------------------------------------------------
;;;
;;; `draw` mutates the returned `lines` in place (via shrink). Components that
;;; return stored content must therefore hand back a fresh copy; span objects are
;;; treated as immutable and may be shared.

(defun copy-line (line)
  "Shallow-copy LINE: a fresh line sharing the (immutable) span objects."
  (%make-line :spans (copy-list (line-spans line))))

(defun copy-lines (lines)
  "Copy LINES so that in-place mutation does not affect the original."
  (make-lines (mapcar #'copy-line (lines-vec lines))))

;;; Component protocol --------------------------------------------------------

(defclass component ()
  ()
  (:documentation "Base class for components: pluggable drawers that output lines
of formatted text and re-render in place at each render."))

(defgeneric draw-unchecked (component dimensions mode)
  (:documentation "Produce the `lines` for COMPONENT given the maximum DIMENSIONS
and the draw MODE (`:normal` or `:final`). Implemented per component."))

(defgeneric draw (component dimensions mode)
  (:documentation "Draw COMPONENT, truncating its output to fit DIMENSIONS.")
  (:method ((component component) dimensions mode)
    (let ((res (draw-unchecked component dimensions mode)))
      (lines-shrink-lines-to-dimensions res dimensions)
      res)))
