;;;; error.lisp
;;;;
;;;; Port of superconsole's error.rs.
;;;;
;;;; Rust models errors as two enums:
;;;;   * `OutputError` - Write / SpawnThread / Terminal, each wrapping an io error.
;;;;   * `Error<D>`     - Draw(D) | Output(OutputError), where `D` is a component's
;;;;                      associated draw-error type.
;;;;
;;;; In Common Lisp we use the condition system. `superconsole-error` is the base
;;;; of the hierarchy. Output errors and draw errors are subclasses; a function
;;;; simply signals whichever condition is appropriate rather than returning a
;;;; tagged `Result`.

(in-package #:supercons)

(define-condition superconsole-error (error)
  ()
  (:documentation "Base condition for all supercons errors."))

;;; Output errors -------------------------------------------------------------

(define-condition output-error (superconsole-error)
  ((cause :initarg :cause :initform nil :reader output-error-cause
          :documentation "The underlying condition that triggered this error, if any."))
  (:documentation "Base for errors writing to or interacting with output streams.
Mirrors Rust's `OutputError` enum."))

(define-condition output-error-write (output-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Error writing to output stream: ~a"
                     (output-error-cause condition))))
  (:documentation "Failure while writing to an output stream. Rust: OutputError::Write."))

(define-condition output-error-spawn-thread (output-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Error spawning thread: ~a"
                     (output-error-cause condition))))
  (:documentation "Failure spawning a worker thread. Rust: OutputError::SpawnThread."))

(define-condition output-error-terminal (output-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Error interacting with terminal: ~a"
                     (output-error-cause condition))))
  (:documentation "Failure interacting with the terminal. Rust: OutputError::Terminal."))

;;; Draw errors ---------------------------------------------------------------

(define-condition draw-error (superconsole-error)
  ((message :initarg :message :initform nil :reader draw-error-message
            :documentation "Human-readable description of the draw failure.")
   (cause :initarg :cause :initform nil :reader draw-error-cause
          :documentation "The underlying condition, if this wraps another."))
  (:report (lambda (condition stream)
             (let ((message (draw-error-message condition))
                   (cause (draw-error-cause condition)))
               (cond (message (write-string message stream))
                     (cause (format stream "~a" cause))
                     (t (write-string "draw error" stream))))))
  (:documentation "Error produced while a component draws itself.
Corresponds to the `Draw(D)` arm of Rust's `Error<D>`. An `output-error`
signalled during drawing corresponds to the `Output` arm; both are
`superconsole-error`s, so a single handler can catch either."))

;;; Constructors --------------------------------------------------------------

(defun output-error-write* (cause)
  "Build an `output-error-write` wrapping CAUSE."
  (make-condition 'output-error-write :cause cause))

(defun output-error-spawn-thread* (cause)
  "Build an `output-error-spawn-thread` wrapping CAUSE."
  (make-condition 'output-error-spawn-thread :cause cause))

(defun output-error-terminal* (cause)
  "Build an `output-error-terminal` wrapping CAUSE."
  (make-condition 'output-error-terminal :cause cause))
