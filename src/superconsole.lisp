;;;; superconsole.lisp
;;;;
;;;; Port of superconsole.rs: the core rendering engine. A canvas at the bottom
;;;; of the terminal is re-rendered in place each tick, while emitted log lines
;;;; scroll above it. Buffers are strings (text + ANSI) rather than byte vectors.

(in-package #:supercons)

(defconstant +minimum-emit+ 5)
(defconstant +max-grapheme-buffer+ 1000000)

;;; ANSI helpers (crossterm command replacements) -----------------------------

(defun ansi-hide-cursor () (format nil "~c[?25l" #\Escape))
(defun ansi-show-cursor () (format nil "~c[?25h" #\Escape))
(defun ansi-move-up (n) (format nil "~c[~dA" #\Escape n))
(defun ansi-move-to-column-0 () (format nil "~c[1G" #\Escape))
(defun ansi-clear-from-cursor-down () (format nil "~c[J" #\Escape))

;;; SuperConsole --------------------------------------------------------------

(defstruct (superconsole (:constructor %make-superconsole))
  (canvas-contents (make-lines) :type lines)
  (to-emit (make-lines) :type lines)
  (aux-to-emit (make-lines) :type lines)
  (fallback-size nil)
  (output nil))

(defun make-superconsole-with-output (fallback-size output)
  "Build a console with an explicit OUTPUT backend and optional FALLBACK-SIZE."
  (%make-superconsole :fallback-size fallback-size :output output))

(defun term-dumb-p ()
  (equal (uiop:getenv "TERM") "dumb"))

(defun superconsole-compatible-p ()
  "True when stderr is a tty and the terminal supports the needed control codes."
  (and (stream-is-tty-p *error-output*) (not (term-dumb-p))))

(defun make-superconsole ()
  "Build a console writing to stderr/stdout if stderr is a compatible tty, else NIL."
  (when (superconsole-compatible-p)
    (make-superconsole-with-output nil (make-blocking-superconsole-output))))

(defun make-superconsole-forced (fallback-size)
  "Build a console regardless of tty compatibility, using FALLBACK-SIZE."
  (make-superconsole-with-output fallback-size (make-blocking-superconsole-output)))

(defun %lines-total-len (lines)
  (reduce #'+ (lines-vec lines) :key #'line-len :initial-value 0))

(defun %sc-size (sc)
  "Determine the drawing size, honoring testing env vars and the fallback."
  (let ((w (uiop:getenv "SUPERCONSOLE_TESTING_WIDTH")))
    (if w
        (make-dimensions (parse-integer w)
                         (parse-integer (uiop:getenv "SUPERCONSOLE_TESTING_HEIGHT")))
        (let ((fallback (superconsole-fallback-size sc)))
          (handler-case
              (let ((size (terminal-size (superconsole-output sc))))
                (if (and fallback (or (zerop (dimensions-width size))
                                      (zerop (dimensions-height size))))
                    fallback
                    size))
            (output-error (e) (if fallback fallback (error e))))))))

;;; Emit / clear --------------------------------------------------------------

(defun superconsole-emit (sc lines)
  "Queue LINES to be drawn above the canvas on the next render."
  (lines-extend (superconsole-to-emit sc) (lines-vec lines)))

(defun superconsole-emit-aux (sc lines)
  "Queue auxiliary LINES to be drawn on the next render."
  (lines-extend (superconsole-aux-to-emit sc) (lines-vec lines)))

(defun %clear-canvas-pre (buffer height)
  (when (> height 0) (write-string (ansi-move-up height) buffer))
  (write-string (ansi-move-to-column-0) buffer))

(defun %clear-canvas-post (buffer)
  (write-string (ansi-clear-from-cursor-down) buffer))

(defun superconsole-clear (sc)
  "Clear the canvas portion of the console."
  (let ((buffer (make-string-output-stream)))
    (%clear-canvas-pre buffer (lines-len (superconsole-canvas-contents sc)))
    (setf (superconsole-canvas-contents sc) (make-lines))
    (%clear-canvas-post buffer)
    (output (superconsole-output sc) (get-output-stream-string buffer))))

;;; Rendering -----------------------------------------------------------------

(defun superconsole-render-general (sc root mode size)
  "Render ROOT into the canvas at SIZE, draining a frame of emitted lines."
  (let ((canvas (draw root size mode)))
    (lines-shrink-lines-to-dimensions canvas size)
    (flet ((compute-limit ()
             (if (and (eq mode :normal)
                      (<= (+ (%lines-total-len (superconsole-to-emit sc))
                             (%lines-total-len (superconsole-aux-to-emit sc)))
                          +max-grapheme-buffer+))
                 (max (max 0 (- (dimensions-height size) (lines-len canvas)))
                      +minimum-emit+)
                 nil)))
      (let* ((limit (compute-limit))
             (reuse-prefix (if (and (lines-empty-p (superconsole-to-emit sc))
                                    (lines-empty-p (superconsole-aux-to-emit sc)))
                               (lines-equal (superconsole-canvas-contents sc) canvas)
                               0))
             (buffer (make-string-output-stream)))
        (write-string (ansi-hide-cursor) buffer)
        (%clear-canvas-pre buffer (- (lines-len (superconsole-canvas-contents sc)) reuse-prefix))
        (unless (lines-empty-p (superconsole-aux-to-emit sc))
          (if (aux-stream-is-tty (superconsole-output sc))
              (progn
                ;; Flush main output first so ordering with the aux tty is correct.
                (output (superconsole-output sc) (get-output-stream-string buffer))
                (let ((aux-buffer (make-string-output-stream)))
                  (setf limit (lines-render-with-limit (superconsole-aux-to-emit sc)
                                                       aux-buffer limit))
                  (output-to (superconsole-output sc)
                             (get-output-stream-string aux-buffer) :aux))
                (setf buffer (make-string-output-stream)))
              (progn
                (let ((output-buffer (make-string-output-stream)))
                  (lines-render-raw (superconsole-aux-to-emit sc) output-buffer)
                  (output-to (superconsole-output sc)
                             (get-output-stream-string output-buffer) :aux))
                (setf limit (compute-limit)))))
        (lines-render-with-limit (superconsole-to-emit sc) buffer limit)
        (lines-render-from-line canvas buffer reuse-prefix)
        (write-string (ansi-show-cursor) buffer)
        (%clear-canvas-post buffer)
        (setf (superconsole-canvas-contents sc) canvas)
        (output (superconsole-output sc) (get-output-stream-string buffer)))))
  (values))

(defun superconsole-render-with-mode (sc root mode)
  "Render ROOT, reserving the always-blank final line."
  (let ((size (dimensions-saturating-sub (%sc-size sc) 1 :vertical)))
    (superconsole-render-general sc root mode size)))

(defun superconsole-render (sc root)
  "Render at a tick: draw ROOT and drain pending emitted events above the canvas."
  (let ((anything-emitted t) (has-rendered nil))
    (loop while (or (not has-rendered)
                    (and anything-emitted
                         (not (and (lines-empty-p (superconsole-to-emit sc))
                                   (lines-empty-p (superconsole-aux-to-emit sc))))))
          do (unless (should-render (superconsole-output sc)) (return))
             (let ((last-len (lines-len (superconsole-to-emit sc)))
                   (last-aux-len (lines-len (superconsole-aux-to-emit sc))))
               (superconsole-render-with-mode sc root :normal)
               (setf anything-emitted
                     (and (= last-len (lines-len (superconsole-to-emit sc)))
                          (= last-aux-len (lines-len (superconsole-aux-to-emit sc)))))
               (setf has-rendered t))))
  (values))

(defun superconsole-emit-now (sc lines root)
  "Queue LINES and immediately re-render ROOT."
  (superconsole-emit sc lines)
  (superconsole-render sc root))

(defun superconsole-finalize (sc root &optional (mode :final))
  "Perform a final render with MODE, then finalize and release the output."
  (superconsole-render-with-mode sc root mode)
  (let ((out (superconsole-output sc)))
    (setf (superconsole-output sc) nil)
    (finalize out)))
