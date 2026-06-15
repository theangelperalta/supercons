;;;; builder.lisp
;;;;
;;;; Port of builder.rs: a builder for constructing a SuperConsole with options
;;;; (non-blocking I/O, custom output stream).

(in-package #:supercons)

(defclass builder ()
  ((non-blocking :initform nil :accessor builder-non-blocking-p)
   (stream :initform *error-output* :accessor builder-stream)
   (aux-stream :initform *standard-output* :accessor builder-aux-stream))
  (:documentation "Builder for a SuperConsole."))

(defun make-builder ()
  "Create a new builder with default options."
  (make-instance 'builder))

(defun builder-non-blocking (builder)
  "Enable non-blocking I/O. Returns BUILDER for chaining."
  (setf (builder-non-blocking-p builder) t)
  builder)

(defun builder-write-to (builder stream)
  "Write the main output to STREAM. Returns BUILDER for chaining."
  (setf (builder-stream builder) stream)
  builder)

(defun %builder-output (builder)
  (if (builder-non-blocking-p builder)
      (make-non-blocking-superconsole-output :stream (builder-stream builder)
                                             :aux-stream (builder-aux-stream builder))
      (make-blocking-superconsole-output :stream (builder-stream builder)
                                         :aux-stream (builder-aux-stream builder))))

(defun builder-build (builder)
  "Build a SuperConsole if stderr is a compatible tty, otherwise NIL."
  (when (superconsole-compatible-p)
    (make-superconsole-with-output nil (%builder-output builder))))

(defun builder-build-forced (builder fallback-size)
  "Build a SuperConsole regardless of tty compatibility, using FALLBACK-SIZE."
  (make-superconsole-with-output fallback-size (%builder-output builder)))
