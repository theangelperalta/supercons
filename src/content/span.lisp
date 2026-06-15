;;;; content/span.lisp
;;;;
;;;; Port of content/span.rs. A `span` is a styled segment of text. It must be
;;;; "valid" -- containing no non-space whitespace -- to be constructed via the
;;;; checked constructors, since superconsole assumes monospaced single-line
;;;; content.

(in-package #:supercons)

;;; Span errors ---------------------------------------------------------------

(define-condition span-error (superconsole-error)
  ()
  (:documentation "Base for span construction errors. Rust: SpanError."))

(define-condition span-error-invalid-whitespace (span-error)
  ((word :initarg :word :initform nil :reader span-error-word))
  (:report (lambda (condition stream)
             (format stream "Word ~a contains non-space whitespace"
                     (span-error-word condition))))
  (:documentation "Signalled when span content contains non-space whitespace.
Rust: SpanError::InvalidWhitespace."))

;;; Hyperlink -----------------------------------------------------------------

(defstruct (hyperlink (:constructor make-hyperlink (uri)))
  (uri "" :type string))

(defun hyperlink= (a b)
  "Equality for optional hyperlinks (compares URIs)."
  (cond ((and (null a) (null b)) t)
        ((and a b) (string= (hyperlink-uri a) (hyperlink-uri b)))
        (t nil)))

;;; Validity / sanitization ---------------------------------------------------

(defun %whitespace-p (ch)
  "True when CH is whitespace per (an approximation of) Rust's char::is_whitespace."
  (let ((code (char-code ch)))
    (or (member code '(9 10 11 12 13 32 #x85 #xA0 #x1680
                       #x2028 #x2029 #x202F #x205F #x3000))
        (and (>= code #x2000) (<= code #x200A)))))

(defun char-valid-p (ch)
  "A char may appear in a span iff it is a space or not whitespace."
  (or (char= ch #\Space) (not (%whitespace-p ch))))

(defun sanitize (stringlike)
  "Strip invalid characters from STRINGLIKE (coerced via princ-to-string)."
  (remove-if-not #'char-valid-p
                 (if (stringp stringlike) stringlike (princ-to-string stringlike))))

(defun span-valid-p (stringlike)
  "True when STRINGLIKE contains only span-valid characters."
  (let ((s (if (stringp stringlike) stringlike (princ-to-string stringlike))))
    (every #'char-valid-p s)))

;;; Span ----------------------------------------------------------------------

(defstruct (span (:constructor %make-span (content style hyperlink))
                 (:predicate spanp))
  (content "" :type string)
  (style (make-content-style) :type content-style)
  (hyperlink nil :type (or null hyperlink)))

(defun span-dash ()
  "An unstyled span containing a single dash."
  (%make-span "-" (make-content-style) nil))

(defun span-sanitized (stringlike)
  "Build an unstyled span, stripping any invalid characters."
  (%make-span (sanitize stringlike) (make-content-style) nil))

(defun span-padding (amount)
  "An unstyled span of AMOUNT spaces."
  (%make-span (make-string amount :initial-element #\Space) (make-content-style) nil))

(defun span-mergeable-with-p (a b)
  "True when A and B differ only in content (same style and hyperlink)."
  (and (content-style= (span-style a) (span-style b))
       (hyperlink= (span-hyperlink a) (span-hyperlink b))))

(defun %coerce-string (stringlike)
  (if (stringp stringlike) stringlike (princ-to-string stringlike)))

(defun make-span-unstyled (stringlike)
  "Create an unstyled span, signalling `span-error-invalid-whitespace` if invalid."
  (let ((s (%coerce-string stringlike)))
    (if (span-valid-p s)
        (%make-span s (make-content-style) nil)
        (error 'span-error-invalid-whitespace :word s))))

(defun make-span-unstyled-lossy (stringlike)
  "Create an unstyled span, sanitizing invalid characters."
  (%make-span (sanitize stringlike) (make-content-style) nil))

(defun make-span-styled (styled)
  "Create a styled span from a styled-content, signalling on invalid content."
  (let ((content (styled-content-content styled)))
    (if (span-valid-p content)
        (%make-span content (copy-content-style* (styled-content-style styled)) nil)
        (error 'span-error-invalid-whitespace :word content))))

(defun make-span-styled-lossy (styled)
  "Create a styled span from a styled-content, sanitizing invalid content."
  (%make-span (sanitize (styled-content-content styled))
              (copy-content-style* (styled-content-style styled)) nil))

(defun make-span-colored (text color)
  "Create a span with foreground COLOR, signalling on invalid TEXT."
  (make-span-styled (make-styled-content (make-content-style :foreground-color color) text)))

(defun make-span-colored-lossy (text color)
  "Create a span with foreground COLOR, sanitizing TEXT."
  (make-span-styled-lossy (make-styled-content (make-content-style :foreground-color color) text)))

(defun span-with-hyperlink (span hyperlink)
  "Return a copy of SPAN with HYPERLINK (or nil) applied."
  (%make-span (span-content span) (copy-content-style* (span-style span)) hyperlink))

(defun span-len (span)
  "Number of terminal columns occupied by SPAN's content."
  (string-column-width (span-content span)))

(defun span-empty-p (span)
  "True when SPAN has no content."
  (zerop (length (span-content span))))

(defun span-graphemes (span)
  "List of single-grapheme spans, each carrying SPAN's style and hyperlink."
  (let ((style (span-style span))
        (hy (span-hyperlink span)))
    (mapcar (lambda (g)
              (%make-span g (copy-content-style* style)
                          (and hy (make-hyperlink (hyperlink-uri hy)))))
            (string-graphemes (span-content span)))))

(defun span= (a b)
  "Structural equality for spans."
  (and (string= (span-content a) (span-content b))
       (content-style= (span-style a) (span-style b))
       (hyperlink= (span-hyperlink a) (span-hyperlink b))))

(defun span-render (span out)
  "Write SPAN to stream OUT as text with ANSI escape codes."
  (unless (span-empty-p span)
    (let* ((style (span-style span))
           (bg (content-style-background-color style))
           (fg (content-style-foreground-color style))
           (attrs (content-style-attributes style))
           (hy (span-hyperlink span))
           (reset-bg nil) (reset-fg nil) (reset-hy nil) (reset nil))
      (when bg (write-string (set-background-color-ansi bg) out) (setf reset-bg t))
      (when fg (write-string (set-foreground-color-ansi fg) out) (setf reset-fg t))
      (when attrs (write-string (set-attributes-ansi attrs) out) (setf reset t))
      (when hy
        (format out "~c]8;;~a~c\\" #\Escape (hyperlink-uri hy) #\Escape)
        (setf reset-hy t))
      (write-string (span-content span) out)
      (when reset-hy (format out "~c]8;;~c\\" #\Escape #\Escape))
      (cond (reset (write-string (reset-color-ansi) out))
            (t (when reset-bg (write-string (set-background-color-ansi :reset) out))
               (when reset-fg (write-string (set-foreground-color-ansi :reset) out))))))
  out)

(defun span-fmt-for-test (span)
  "Render SPAN as a debug string (matches Rust's `fmt_for_test`)."
  (let ((style (span-style span)))
    (if (content-style-default-p style)
        (span-content span)
        (with-output-to-string (f)
          (write-string "<span" f)
          (let ((fg (content-style-foreground-color style)))
            (when fg (format f " fg=~a" (color-fmt-for-test fg))))
          (let ((bg (content-style-background-color style)))
            (when bg (format f " bg=~a" (color-fmt-for-test bg))))
          (dolist (attr (content-style-attributes style))
            (format f " ~a" (cddr (assoc attr *attributes*))))
          (write-string ">" f)
          (write-string (span-content span) f)
          (write-string "</span>" f)))))
