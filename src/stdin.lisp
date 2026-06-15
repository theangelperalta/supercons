;;;; stdin.lisp
;;;;
;;;; Port of stdin.rs: a non-blocking stdin reader backed by a blocking
;;;; background thread. Rust uses tokio's AsyncRead; here a worker thread reads
;;;; chunks and forwards them through a lock/condition-variable queue, which
;;;; callers drain (optionally without blocking). Reading is deferred until the
;;;; first read, matching the upstream behavior.

(in-package #:supercons)

(defclass stdin-reader ()
  ((stream :initarg :stream :accessor stdin-stream)
   (buffer-size :initarg :buffer-size :accessor stdin-buffer-size)
   (lock :initform (bt:make-lock) :accessor stdin-lock)
   (cv :initform (bt:make-condition-variable) :accessor stdin-cv)
   (queue :initform '() :accessor stdin-queue)
   (eof :initform nil :accessor stdin-eof-p)
   (error :initform nil :accessor stdin-error)
   (thread :initform nil :accessor stdin-thread)
   (started :initform nil :accessor stdin-started-p))
  (:documentation "A non-blocking reader for STREAM, fed by a background thread."))

(defun make-stdin-reader (&key (buffer-size 8192) (stream *standard-input*))
  "Create a stdin reader. BUFFER-SIZE controls the worker read buffer."
  (make-instance 'stdin-reader :buffer-size buffer-size :stream stream))

(defun %stdin-worker (reader)
  (let ((buf (make-string (stdin-buffer-size reader))))
    (handler-case
        (loop
          (let ((n (read-sequence buf (stdin-stream reader))))
            (if (zerop n)
                (progn
                  (bt:with-lock-held ((stdin-lock reader))
                    (setf (stdin-eof-p reader) t)
                    (bt:condition-notify (stdin-cv reader)))
                  (return))
                (bt:with-lock-held ((stdin-lock reader))
                  (setf (stdin-queue reader)
                        (append (stdin-queue reader) (list (subseq buf 0 n))))
                  (bt:condition-notify (stdin-cv reader))))))
      (error (e)
        (bt:with-lock-held ((stdin-lock reader))
          (setf (stdin-error reader) e
                (stdin-eof-p reader) t)
          (bt:condition-notify (stdin-cv reader)))))))

(defun %stdin-ensure-started (reader)
  "Spawn the worker thread on first use (deferred reading)."
  (bt:with-lock-held ((stdin-lock reader))
    (unless (stdin-started-p reader)
      (setf (stdin-started-p reader) t
            (stdin-thread reader)
            (bt:make-thread (lambda () (%stdin-worker reader))
                            :name "superconsole-stdin")))))

(defun stdin-read-chunk (reader &key (wait t))
  "Return the next chunk of input as a string, or NIL on EOF (or when WAIT is
NIL and no data is currently available). Signals any read error encountered."
  (%stdin-ensure-started reader)
  (bt:with-lock-held ((stdin-lock reader))
    (block result
      (loop
        (when (stdin-queue reader)
          (return-from result (pop (stdin-queue reader))))
        (when (stdin-error reader)
          (let ((e (stdin-error reader)))
            (setf (stdin-error reader) nil)
            (error e)))
        (when (stdin-eof-p reader)
          (return-from result nil))
        (unless wait
          (return-from result nil))
        (bt:condition-wait (stdin-cv reader) (stdin-lock reader))))))
