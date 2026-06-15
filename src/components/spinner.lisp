;;;; components/spinner.lisp
;;;;
;;;; Port of components/spinner.rs. A spinner animates through a set of glyphs
;;;; based on a `tick` value and displays a message alongside it.

(in-package #:supercons)

(defparameter +braille-spinner+
  (coerce (mapcar #'code-char
                  '(#x280B #x2819 #x2839 #x2838 #x283C #x2834 #x2826 #x2827 #x2807 #x280F))
          'vector)
  "Default braille spinner glyphs used for animation.")

(defclass spinner (component)
  ((tick :initarg :tick :accessor spinner-tick)
   (message :initarg :message :accessor spinner-message)
   (chars :initarg :chars :accessor spinner-chars))
  (:documentation "A component that renders an animated spinner with a message."))

(defun make-spinner (tick message &key (chars +braille-spinner+))
  "Construct a spinner at TICK showing MESSAGE (a line), using CHARS (a vector)."
  (make-instance 'spinner :tick tick :message message :chars chars))

(defun spinner-current-char (spinner)
  "The glyph for SPINNER's current tick, or space when there are no glyphs."
  (let ((chars (spinner-chars spinner)))
    (if (zerop (length chars))
        #\Space
        (aref chars (mod (spinner-tick spinner) (length chars))))))

(defmethod draw-unchecked ((spinner spinner) dimensions mode)
  (declare (ignore dimensions))
  (ecase mode
    (:final
     (make-lines (list (copy-line (spinner-message spinner)))))
    (:normal
     (let ((line (make-line (list (span-sanitized
                                   (format nil "~c " (spinner-current-char spinner)))))))
       (dolist (span (line-spans (spinner-message spinner)))
         (line-push line span))
       (make-lines (list line))))))
