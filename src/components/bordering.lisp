;;;; components/bordering.lisp
;;;;
;;;; Port of components/bordering.rs. `bordered` draws borders on each side of
;;;; its child's output. Borders may be any span (possibly multi-column).

(in-package #:supercons)

(defstruct (bordered-spec (:constructor %make-bordered-spec (left right top bottom)))
  ;; Each side is either a span (the border) or NIL (no border).
  (left nil) (right nil) (top nil) (bottom nil))

(defun %resolve-border (value default-char)
  "Resolve a border argument: :default -> a span of DEFAULT-CHAR, NIL -> no
border, a string -> an unstyled span, otherwise the span itself."
  (cond ((eq value :default) (make-span-unstyled default-char))
        ((null value) nil)
        ((stringp value) (make-span-unstyled value))
        (t value)))

(defun make-bordered-spec (&key (left :default) (right :default)
                                (top :default) (bottom :default))
  "Construct a `bordered-spec`. Unspecified sides default to '|' (left/right) or
'-' (top/bottom); pass NIL for a side to omit its border."
  (%make-bordered-spec (%resolve-border left "|")
                       (%resolve-border right "|")
                       (%resolve-border top "-")
                       (%resolve-border bottom "-")))

(defclass bordered (component)
  ((child :initarg :child :accessor bordered-child)
   (spec :initarg :spec :accessor bordered-spec))
  (:documentation "Surrounds CHILD's output with borders per SPEC."))

(defun make-bordered (child &key (spec (make-bordered-spec)))
  "Construct a `bordered`. CHILD is wrapped in a justified, top-aligned box."
  (make-instance 'bordered
                 :child (make-aligned child :horizontal :left-justified :vertical :top)
                 :spec spec))

(defun construct-vertical-padding (padding width)
  "Transpose horizontal PADDING into a list of full-WIDTH lines, one per grapheme."
  (mapcar (lambda (gspan)
            (let* ((slen (span-len gspan))
                   (reps (if (zerop slen) 0 (floor width slen)))
                   (content (apply #'concatenate 'string
                                   (make-list reps :initial-element (span-content gspan)))))
              (make-line (list (%make-span content (span-style gspan) (span-hyperlink gspan))))))
          (span-graphemes padding)))

(defmethod draw-unchecked ((component bordered) dimensions mode)
  (let* ((spec (bordered-spec component))
         (width (dimensions-width dimensions))
         (height (dimensions-height dimensions))
         (left (bordered-spec-left spec))
         (right (bordered-spec-right spec))
         (top (bordered-spec-top spec))
         (bottom (bordered-spec-bottom spec)))
    (flet ((olen (s) (if s (span-len s) 0)))
      (let* ((new-dims (make-dimensions (max 0 (- width (+ (olen left) (olen right))))
                                        (max 0 (- height (+ (olen top) (olen bottom))))))
             (output (draw (bordered-child component) new-dims mode)))
        (dolist (line (lines-vec output))
          (when left (line-push-front line left))
          (when right (line-push line right)))
        (when top
          (setf (lines-vec output)
                (append (construct-vertical-padding top (lines-max-line-length output))
                        (lines-vec output))))
        (when bottom
          (lines-extend output (construct-vertical-padding bottom (lines-max-line-length output))))
        output))))
