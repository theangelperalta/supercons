;;;; output.lisp
;;;;
;;;; Port of output.rs: the `OutputTarget` enum, the `SuperConsoleOutput`
;;;; protocol, and the blocking / non-blocking output implementations.
;;;;
;;;; The Rust buffers are `Vec<u8>`; since we deal in text + ANSI escapes we use
;;;; strings throughout. TTY detection and terminal-size queries (provided by
;;;; crossterm in Rust) are implemented here via small libc FFI calls.

(in-package #:supercons)

;;; Output target -------------------------------------------------------------

(defconstant +output-target-main+ :main)
(defconstant +output-target-aux+ :aux)

(deftype output-target () '(member :main :aux))

;;; libc FFI for tty detection / terminal size --------------------------------

(sb-alien:define-alien-routine ("isatty" %c-isatty) sb-alien:int
  (fd sb-alien:int))

(sb-alien:define-alien-routine ("ioctl" %c-ioctl) sb-alien:int
  (fd sb-alien:int)
  (request sb-alien:unsigned-long)
  (arg sb-alien:system-area-pointer))

(defparameter +tiocgwinsz+
  #+darwin #x40087468
  #+linux #x5413
  #-(or darwin linux) nil
  "The TIOCGWINSZ ioctl request number for this platform, or NIL if unknown.")

(defun %stream-fd (stream)
  "Best-effort retrieval of the file descriptor backing STREAM, or NIL."
  (ignore-errors
   (typecase stream
     (sb-sys:fd-stream (sb-sys:fd-stream-fd stream))
     (synonym-stream (%stream-fd (symbol-value (synonym-stream-symbol stream))))
     (two-way-stream (%stream-fd (two-way-stream-output-stream stream)))
     (t nil))))

(defun stream-is-tty-p (stream)
  "True when STREAM is connected to a terminal."
  (let ((fd (%stream-fd stream)))
    (and fd (handler-case (= 1 (%c-isatty fd)) (error () nil)))))

(defun query-terminal-size ()
  "Best-effort terminal size as `dimensions`, or NIL if it cannot be determined.
Tries the COLUMNS/LINES environment variables, then a TIOCGWINSZ ioctl."
  (let ((cols (ignore-errors (parse-integer (or (sb-ext:posix-getenv "COLUMNS") "")
                                            :junk-allowed t)))
        (rows (ignore-errors (parse-integer (or (sb-ext:posix-getenv "LINES") "")
                                            :junk-allowed t))))
    (cond
      ((and cols rows (plusp cols) (plusp rows)) (make-dimensions cols rows))
      (+tiocgwinsz+
       (ignore-errors
        (let ((ws (sb-alien:make-alien sb-alien:unsigned-short 4)))
          (unwind-protect
               (when (zerop (%c-ioctl 2 +tiocgwinsz+ (sb-alien:alien-sap ws)))
                 (let ((r (sb-alien:deref ws 0)) (c (sb-alien:deref ws 1)))
                   (when (and (plusp r) (plusp c)) (make-dimensions c r))))
            (sb-alien:free-alien ws)))))
      (t nil))))

;;; SuperConsoleOutput protocol -----------------------------------------------

(defgeneric should-render (output)
  (:documentation "Return true if rendering should proceed; may veto a render."))

(defgeneric output (output buffer)
  (:documentation "Emit BUFFER (a string). Flushes if possible."))

(defgeneric output-to (output buffer target)
  (:documentation "Emit BUFFER to TARGET (`:main` or `:aux`)."))

(defgeneric aux-stream-is-tty (output)
  (:documentation "True if the auxiliary stream is a tty."))

(defgeneric terminal-size (output)
  (:documentation "Size of the terminal as `dimensions`."))

(defgeneric finalize (output)
  (:documentation "Called when the console is finalized; must block as needed."))

(defclass superconsole-output () ()
  (:documentation "Base class for output backends."))

;; Defaults mirroring the Rust trait's provided methods.
(defmethod output-to ((o superconsole-output) buffer target)
  (declare (ignore target))
  (output o buffer))

(defmethod aux-stream-is-tty ((o superconsole-output))
  (declare (ignore o))
  t)

(defmethod terminal-size ((o superconsole-output))
  (declare (ignore o))
  (or (query-terminal-size)
      (error 'output-error-terminal :cause "could not determine terminal size")))

;;; Blocking output -----------------------------------------------------------

(defclass blocking-superconsole-output (superconsole-output)
  ((stream :initarg :stream :accessor bsco-stream)
   (aux-stream :initarg :aux-stream :accessor bsco-aux-stream))
  (:documentation "Writes synchronously to its streams."))

(defun make-blocking-superconsole-output (&key (stream *error-output*)
                                               (aux-stream *standard-output*))
  "Construct a blocking output writing to STREAM (main) and AUX-STREAM."
  (make-instance 'blocking-superconsole-output :stream stream :aux-stream aux-stream))

(defmethod should-render ((o blocking-superconsole-output)) t)

(defmethod output ((o blocking-superconsole-output) buffer)
  (output-to o buffer :main))

(defmethod output-to ((o blocking-superconsole-output) buffer target)
  (handler-case
      (let ((stream (ecase target
                      (:main (bsco-stream o))
                      (:aux (bsco-aux-stream o)))))
        (write-string buffer stream)
        (finish-output stream))
    (error (e) (error 'output-error-write :cause e))))

(defmethod aux-stream-is-tty ((o blocking-superconsole-output))
  (stream-is-tty-p (bsco-aux-stream o)))

(defmethod finalize ((o blocking-superconsole-output)) nil)

;;; Non-blocking output -------------------------------------------------------
;;;
;;; A background thread performs the writes. We allow up to two outstanding
;;; frames (one queued, one being written), matching Rust's bounded(1) channel
;;; plus the in-flight frame. Write errors are collected and surfaced on the
;;; next fallible call, one per call, like Rust's error channel.

(defclass non-blocking-superconsole-output (superconsole-output)
  ((stream :initarg :stream :accessor nbsco-stream)
   (aux-stream :initarg :aux-stream :accessor nbsco-aux-stream)
   (lock :initform (bt:make-lock) :accessor nbsco-lock)
   (cv :initform (bt:make-condition-variable) :accessor nbsco-cv)
   (queue :initform '() :accessor nbsco-queue)
   (pending :initform 0 :accessor nbsco-pending)
   (errors :initform '() :accessor nbsco-errors)
   (stopped :initform nil :accessor nbsco-stopped)
   (thread :initform nil :accessor nbsco-thread)
   (aux-compatible :initarg :aux-compatible :accessor nbsco-aux-compatible))
  (:documentation "Writes asynchronously via a background worker thread."))

(defun %nbsco-write (o buffer target)
  (let ((stream (ecase target
                  (:main (nbsco-stream o))
                  (:aux (nbsco-aux-stream o)))))
    (write-string buffer stream)
    (finish-output stream)))

(defun %nbsco-worker (o)
  (loop
    (let ((item nil) (stop nil))
      (bt:with-lock-held ((nbsco-lock o))
        (loop while (and (null (nbsco-queue o)) (not (nbsco-stopped o)))
              do (bt:condition-wait (nbsco-cv o) (nbsco-lock o)))
        (if (nbsco-queue o)
            (setf item (pop (nbsco-queue o)))
            (setf stop t)))
      (when stop (return))
      (handler-case (%nbsco-write o (car item) (cdr item))
        (error (e)
          (bt:with-lock-held ((nbsco-lock o))
            (setf (nbsco-errors o)
                  (append (nbsco-errors o)
                          (list (make-condition 'output-error-write :cause e)))))))
      (bt:with-lock-held ((nbsco-lock o))
        (decf (nbsco-pending o))
        (bt:condition-notify (nbsco-cv o))))))

(defun make-non-blocking-superconsole-output (&key (stream *error-output*)
                                                   (aux-stream *standard-output*))
  "Construct a non-blocking output and start its worker thread."
  (let ((o (make-instance 'non-blocking-superconsole-output
                          :stream stream :aux-stream aux-stream
                          :aux-compatible (stream-is-tty-p aux-stream))))
    (setf (nbsco-thread o)
          (bt:make-thread (lambda () (%nbsco-worker o)) :name "superconsole-io"))
    o))

(defmethod should-render ((o non-blocking-superconsole-output))
  (bt:with-lock-held ((nbsco-lock o))
    (if (nbsco-errors o) t (< (nbsco-pending o) 2))))

(defmethod output ((o non-blocking-superconsole-output) buffer)
  (output-to o buffer :main))

(defmethod output-to ((o non-blocking-superconsole-output) buffer target)
  (let ((err nil))
    (bt:with-lock-held ((nbsco-lock o))
      (when (nbsco-errors o) (setf err (pop (nbsco-errors o)))))
    (when err (error err))
    (bt:with-lock-held ((nbsco-lock o))
      (incf (nbsco-pending o))
      (setf (nbsco-queue o) (append (nbsco-queue o) (list (cons buffer target))))
      (bt:condition-notify (nbsco-cv o))))
  nil)

(defmethod aux-stream-is-tty ((o non-blocking-superconsole-output))
  (nbsco-aux-compatible o))

(defmethod finalize ((o non-blocking-superconsole-output))
  (bt:with-lock-held ((nbsco-lock o))
    (setf (nbsco-stopped o) t)
    (bt:condition-notify (nbsco-cv o)))
  (bt:join-thread (nbsco-thread o))
  (let ((err (bt:with-lock-held ((nbsco-lock o)) (pop (nbsco-errors o)))))
    (when err (error err))
    nil))
