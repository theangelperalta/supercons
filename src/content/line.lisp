;;;; content/line.lisp
;;;;
;;;; Port of content/line.rs. A `line` is an ordered, normalized collection of
;;;; spans representing one row of text. Normalization invariants: all spans are
;;;; non-empty, and adjacent spans have differing styles (mergeable neighbours
;;;; are concatenated).

(in-package #:supercons)

(defstruct (line (:constructor %make-line) (:copier nil))
  ;; List of spans, kept normalized.
  (spans '() :type list))

(defun make-line (&optional spans)
  "Create a line, pushing each span in SPANS (which normalizes/merges them)."
  (let ((line (%make-line)))
    (dolist (s spans) (line-push line s))
    line))

(defun line-from-spans (spans)
  "Build a line from a list of spans."
  (make-line spans))

(defun line-unstyled (text)
  "Single-span unstyled line; signals `span-error-invalid-whitespace` if invalid."
  (make-line (list (make-span-unstyled text))))

(defun line-sanitized (text)
  "Single-span line built from sanitized TEXT."
  (make-line (list (span-sanitized text))))

(defun line-from-strings (strings)
  "Build a line, mapping each string in STRINGS through `make-span-unstyled`.
Mirrors Rust's TryFrom<Vec<&str>>; adjacent default-styled spans merge."
  (make-line (mapcar #'make-span-unstyled strings)))

(defun line-len (line)
  "Total column width of all spans in LINE."
  (loop for span in (line-spans line) sum (span-len span)))

(defun line-empty-p (line)
  "True when LINE has no spans."
  (null (line-spans line)))

(defun line-push (line span)
  "Append SPAN to LINE, merging with the last span when mergeable, dropping empty."
  (unless (span-empty-p span)
    (let ((spans (line-spans line)))
      (if (and spans (span-mergeable-with-p (first (last spans)) span))
          (let* ((last (first (last spans)))
                 (merged (%make-span (concatenate 'string
                                                  (span-content last) (span-content span))
                                     (span-style last) (span-hyperlink last))))
            (setf (line-spans line) (append (butlast spans) (list merged))))
          (setf (line-spans line) (append spans (list span))))))
  line)

(defun line-push-front (line span)
  "Prepend SPAN to LINE (with the same normalization as `line-push`)."
  (let ((old (line-spans line)))
    (setf (line-spans line) '())
    (line-push line span)
    (dolist (s old) (line-push line s)))
  line)

(defun line-pad-right (line amount)
  "Pad the right of LINE with AMOUNT spaces."
  (line-push line (span-padding amount)))

(defun line-pad-left (line amount)
  "Pad the left of LINE with AMOUNT spaces."
  (line-push-front line (span-padding amount)))

(defun line-truncate-line (line max-width)
  "Truncate the right of LINE (by grapheme count) to at most MAX-WIDTH."
  (let ((result '()) (cur 0))
    (block walk
      (dolist (span (line-spans line))
        (when (>= cur max-width) (return-from walk))
        (let* ((graphs (string-graphemes (span-content span)))
               (wlen (length graphs)))
          (if (> (+ wlen cur) max-width)
              (let ((take (- max-width cur)))
                (push (%make-span (apply #'concatenate 'string (subseq graphs 0 take))
                                  (span-style span) (span-hyperlink span))
                      result)
                (return-from walk))
              (progn (push span result) (incf cur wlen))))))
    (setf (line-spans line) (nreverse result)))
  line)

(defun line-trim-ends (line start width)
  "Keep WIDTH graphemes after dropping the first START graphemes of LINE."
  (let ((spans (line-spans line)))
    (setf (line-spans line) '())
    (block walk
      (dolist (span spans)
        (let* ((graphs (string-graphemes (span-content span)))
               (len (length graphs)))
          (if (and (> start 0) (< len start))
              (decf start len)
              (let* ((end (min len (+ start width)))
                     (newspan (if (or (/= start 0) (/= end len))
                                  (%make-span (apply #'concatenate 'string
                                                     (subseq graphs start end))
                                              (span-style span) (span-hyperlink span))
                                  span)))
                (line-push line newspan)
                (decf width (- end start))
                (setf start 0)
                (when (= width 0) (return-from walk))))))))
  line)

(defun line-to-exact-width (line exact-width)
  "Pad or truncate the right of LINE so its column width equals EXACT-WIDTH."
  (let ((len (line-len line)))
    (cond ((< len exact-width) (line-pad-right line (- exact-width len)))
          ((> len exact-width) (line-truncate-line line exact-width))))
  line)

(defun line-render (line)
  "Render LINE to a string with ANSI escape codes (no trailing newline)."
  (with-output-to-string (out)
    (dolist (span (line-spans line)) (span-render span out))))

(defun line-render-with-clear-and-nl (line out)
  "Render LINE to stream OUT followed by clear-to-EOL, newline, and column reset."
  (dolist (span (line-spans line)) (span-render span out))
  (format out "~c[K~%~c[1G" #\Escape #\Escape))

(defun line-to-unstyled (line)
  "Concatenate the content of all spans, discarding styling."
  (apply #'concatenate 'string (mapcar #'span-content (line-spans line))))

(defun line= (a b)
  "Structural equality for lines."
  (let ((sa (line-spans a)) (sb (line-spans b)))
    (and (= (length sa) (length sb)) (every #'span= sa sb))))

(defun line-fmt-for-test (line)
  "Render LINE as a debug string by concatenating each span's `fmt-for-test`."
  (with-output-to-string (out)
    (dolist (span (line-spans line))
      (write-string (span-fmt-for-test span) out))))
