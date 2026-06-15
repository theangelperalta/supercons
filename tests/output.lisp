;;;; tests/output.lisp -- ports of output.rs tests, plus basic output coverage.

(in-package #:supercons/tests)


;;; A character output stream whose writes block until released, then may fail.
;;; Mirrors the TestWriter rendezvous used by the Rust non-blocking test.
(defclass blocking-writer (sb-gray:fundamental-character-output-stream)
  ((entered :initform (bt:make-semaphore) :accessor bw-entered)
   (release :initform (bt:make-semaphore) :accessor bw-release)
   (fail :initform nil :accessor bw-fail)))

(defmethod sb-gray:stream-write-string ((s blocking-writer) string &optional start end)
  (declare (ignore start end))
  (bt:signal-semaphore (bw-entered s))
  (bt:wait-on-semaphore (bw-release s) :timeout 5)
  (when (bw-fail s) (error "not writable"))
  string)

(defmethod sb-gray:stream-write-char ((s blocking-writer) ch) ch)
(defmethod sb-gray:stream-line-column ((s blocking-writer)) nil)

(defun run-nonblocking-scenario (target0 target1)
  "Replicates output.rs::test_non_blocking_output_errors_on_next_output for the
given pair of output targets."
  (let* ((writer (make-instance 'blocking-writer))
         (output (sc:make-non-blocking-superconsole-output :stream writer :aux-stream writer)))
    (ok (sc:should-render output))
    (sc:output-to output "x" target0)
    ;; Worker is now blocked inside the first write.
    (ok (bt:wait-on-semaphore (bw-entered writer) :timeout 5))
    ;; Second message stays queued; we now have two outstanding frames.
    (sc:output-to output "y" target1)
    (ng (sc:should-render output))
    ;; Kill the writer: both pending writes will fail.
    (setf (bw-fail writer) t)
    (bt:signal-semaphore (bw-release writer))
    (ok (bt:wait-on-semaphore (bw-entered writer) :timeout 5))
    (bt:signal-semaphore (bw-release writer))
    ;; should-render eventually becomes true once an error is recorded.
    (loop repeat 5000 until (sc:should-render output) do (sleep 0.001))
    (ok (sc:should-render output))
    ;; Sending output and finalizing both surface errors.
    (ok (signals (sc:output output "") 'sc:output-error))
    (ok (signals (sc:finalize output) 'sc:output-error))))

(deftest nonblocking-output-errors-on-next-output
  (dolist (t0 (list sc:+output-target-main+ sc:+output-target-aux+))
    (dolist (t1 (list sc:+output-target-main+ sc:+output-target-aux+))
      (run-nonblocking-scenario t0 t1))))

(deftest blocking-output-writes
  (let* ((stream (make-string-output-stream))
         (output (sc:make-blocking-superconsole-output :stream stream :aux-stream stream)))
    (ok (sc:should-render output))
    (sc:output output "hello")
    (sc:output-to output " world" sc:+output-target-main+)
    (ok (string= "hello world" (get-output-stream-string stream)))
    (ok (null (sc:finalize output)))))

(deftest nonblocking-output-flushes-on-finalize
  (let* ((stream (make-string-output-stream))
         (output (sc:make-non-blocking-superconsole-output :stream stream :aux-stream stream)))
    (sc:output output "alpha")
    (sc:output output "beta")
    (sc:finalize output)
    (ok (string= "alphabeta" (get-output-stream-string stream)))))
