;;;; testing.lisp
;;;;
;;;; Port of testing.rs: a `test-output` that records frames instead of doing
;;;; real I/O, plus helpers for constructing test consoles and asserting on
;;;; rendered frames.

(in-package #:supercons)

(defun test-output-aux-prefix () "AUX PREFIX: ")

(defun aux-output-with-prefix (content)
  "Prefix CONTENT with the aux marker, matching TestOutput::aux_output_with_prefix."
  (concatenate 'string (test-output-aux-prefix) content))

(defclass test-output (superconsole-output)
  ((should-render :initarg :should-render :initform t :accessor test-output-should-render)
   (terminal-size :initarg :terminal-size :accessor test-output-terminal-size)
   (frames :initform '() :accessor test-output-frames)
   (aux-stream-is-tty :initarg :aux-stream-is-tty :initform t
                      :accessor test-output-aux-stream-is-tty))
  (:documentation "An output for testing that records frames and does no real I/O."))

(defmethod should-render ((o test-output))
  (test-output-should-render o))

(defmethod output ((o test-output) buffer)
  (setf (test-output-frames o) (append (test-output-frames o) (list buffer)))
  nil)

(defmethod output-to ((o test-output) buffer target)
  (ecase target
    (:main (output o buffer))
    (:aux (output o (aux-output-with-prefix buffer)))))

(defmethod aux-stream-is-tty ((o test-output))
  (test-output-aux-stream-is-tty o))

(defmethod terminal-size ((o test-output))
  (test-output-terminal-size o))

(defmethod finalize ((o test-output)) nil)

(defun test-output (sc)
  "Return the `test-output` backing console SC."
  (superconsole-output sc))

(defun %test-console-inner (aux-stream-is-tty)
  (let ((size (make-dimensions 80 80)))
    (make-superconsole-with-output
     size
     (make-instance 'test-output :should-render t :terminal-size size
                                 :aux-stream-is-tty aux-stream-is-tty))))

(defun test-console ()
  "A console backed by a `test-output` with a tty-compatible aux stream."
  (%test-console-inner t))

(defun test-console-aux-incompatible ()
  "A console backed by a `test-output` with a non-tty aux stream."
  (%test-console-inner nil))

(defun frame-contains-p (frame needle)
  "True when string FRAME contains substring NEEDLE."
  (and (search needle frame) t))

(defun assert-frame-contains (frame needle)
  "Signal an error unless FRAME contains NEEDLE."
  (unless (frame-contains-p frame needle)
    (error "Expected frame to contain `~a`, but was:~%~a" needle frame)))
