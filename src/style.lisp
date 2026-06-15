;;;; style.lisp
;;;;
;;;; Port of style.rs, which simply re-exports crossterm's style types. Since we
;;;; depend on raw ANSI rather than crossterm, we reimplement the relevant pieces
;;;; here: `Color`, `Attribute`, `ContentStyle`, `StyledContent`, the `Stylize`
;;;; helper methods, the ANSI escape emitters, and unicode column-width /
;;;; grapheme handling (replacing termwiz + unicode-segmentation).

(in-package #:supercons)

;;; Small text utility --------------------------------------------------------

(defun to-snake-case (string)
  "Convert a CamelCase STRING to snake_case, matching the Rust helper used by
`fmt_for_test`."
  (with-output-to-string (out)
    (loop for ch across string
          for firstp = t then nil
          do (if (upper-case-p ch)
                 (progn (unless firstp (write-char #\_ out))
                        (write-char (char-downcase ch) out))
                 (write-char ch out)))))

;;; Colors --------------------------------------------------------------------
;;;
;;; The 16 named colors are represented as keywords plus `:reset`. RGB and ANSI
;;; (256) colors are small structs. Indices below are the ANSI-256 codes used by
;;; crossterm when emitting `38;5;N` / `48;5;N` sequences.

(defstruct (rgb-color (:constructor make-rgb-color (r g b)))
  (r 0 :type (integer 0 255))
  (g 0 :type (integer 0 255))
  (b 0 :type (integer 0 255)))

(defstruct (ansi-color (:constructor make-ansi-color (value)))
  (value 0 :type (integer 0 255)))

(defparameter *named-colors*
  '((:reset        . (nil . "reset"))
    (:black        . (0   . "black"))
    (:dark-grey    . (8   . "dark_grey"))
    (:red          . (9   . "red"))
    (:dark-red     . (1   . "dark_red"))
    (:green        . (10  . "green"))
    (:dark-green   . (2   . "dark_green"))
    (:yellow       . (11  . "yellow"))
    (:dark-yellow  . (3   . "dark_yellow"))
    (:blue         . (12  . "blue"))
    (:dark-blue    . (4   . "dark_blue"))
    (:magenta      . (13  . "magenta"))
    (:dark-magenta . (5   . "dark_magenta"))
    (:cyan         . (14  . "cyan"))
    (:dark-cyan    . (6   . "dark_cyan"))
    (:white        . (15  . "white"))
    (:grey         . (7   . "grey")))
  "Alist mapping color keyword -> (ANSI-256-index . snake-case-name).")

(defconstant +reset+ :reset)
(defconstant +black+ :black)
(defconstant +dark-grey+ :dark-grey)
(defconstant +red+ :red)
(defconstant +dark-red+ :dark-red)
(defconstant +green+ :green)
(defconstant +dark-green+ :dark-green)
(defconstant +yellow+ :yellow)
(defconstant +dark-yellow+ :dark-yellow)
(defconstant +blue+ :blue)
(defconstant +dark-blue+ :dark-blue)
(defconstant +magenta+ :magenta)
(defconstant +dark-magenta+ :dark-magenta)
(defconstant +cyan+ :cyan)
(defconstant +dark-cyan+ :dark-cyan)
(defconstant +white+ :white)
(defconstant +grey+ :grey)

(deftype color () '(or keyword rgb-color ansi-color))

(defun color= (a b)
  "Structural equality for colors."
  (cond ((and (keywordp a) (keywordp b)) (eq a b))
        ((and (rgb-color-p a) (rgb-color-p b))
         (and (= (rgb-color-r a) (rgb-color-r b))
              (= (rgb-color-g a) (rgb-color-g b))
              (= (rgb-color-b a) (rgb-color-b b))))
        ((and (ansi-color-p a) (ansi-color-p b))
         (= (ansi-color-value a) (ansi-color-value b)))
        (t nil)))

(defun color-fmt-for-test (color)
  "Render COLOR the way Rust's `fmt_for_test` does."
  (etypecase color
    (keyword (cddr (assoc color *named-colors*)))
    (rgb-color (format nil "rgb(~d, ~d, ~d)"
                       (rgb-color-r color) (rgb-color-g color) (rgb-color-b color)))
    (ansi-color (format nil "ansi(~d)" (ansi-color-value color)))))

(defun %color-ansi-body (color)
  "Return the `5;N` / `2;r;g;b` body for a non-reset COLOR."
  (etypecase color
    (keyword (format nil "5;~d" (cadr (assoc color *named-colors*))))
    (rgb-color (format nil "2;~d;~d;~d"
                       (rgb-color-r color) (rgb-color-g color) (rgb-color-b color)))
    (ansi-color (format nil "5;~d" (ansi-color-value color)))))

(defun set-foreground-color-ansi (color)
  "ANSI escape that sets the foreground COLOR (crossterm `SetForegroundColor`)."
  (if (eq color :reset)
      (format nil "~c[39m" #\Escape)
      (format nil "~c[38;~am" #\Escape (%color-ansi-body color))))

(defun set-background-color-ansi (color)
  "ANSI escape that sets the background COLOR (crossterm `SetBackgroundColor`)."
  (if (eq color :reset)
      (format nil "~c[49m" #\Escape)
      (format nil "~c[48;~am" #\Escape (%color-ansi-body color))))

(defun reset-color-ansi ()
  "ANSI escape that resets all colors/attributes (crossterm `ResetColor`)."
  (format nil "~c[0m" #\Escape))

;;; Attributes ----------------------------------------------------------------
;;;
;;; Attributes are kept as a set (list) of keywords. The canonical order below
;;; matches crossterm's `Attribute` enum declaration order, which drives both
;;; `fmt_for_test` output and ANSI emission.

(defparameter *attributes*
  '((:bold        . (1 . "bold"))
    (:dim         . (2 . "dim"))
    (:italic      . (3 . "italic"))
    (:underlined  . (4 . "underlined"))
    (:reverse     . (7 . "reverse"))
    (:hidden      . (8 . "hidden"))
    (:crossed-out . (9 . "crossed_out")))
  "Alist mapping attribute keyword -> (SGR-code . snake-case-name), in canonical
crossterm order.")

(defun attribute-order (attr)
  "Position of ATTR in the canonical order, for sorting."
  (or (position attr *attributes* :key #'car) most-positive-fixnum))

(defun normalize-attributes (attrs)
  "Return ATTRS as a de-duplicated list sorted into canonical order."
  (sort (remove-duplicates (copy-list attrs)) #'< :key #'attribute-order))

(defun attribute-set= (a b)
  "Set equality for two attribute lists."
  (and (= (length a) (length b))
       (null (set-difference a b))
       (null (set-difference b a))))

(defun set-attributes-ansi (attrs)
  "ANSI escape sequence setting all ATTRS (crossterm `SetAttributes`)."
  (with-output-to-string (out)
    (dolist (attr (normalize-attributes attrs))
      (let ((sgr (cadr (assoc attr *attributes*))))
        (when sgr
          (format out "~c[~dm" #\Escape sgr))))))

;;; ContentStyle --------------------------------------------------------------

(defstruct (content-style (:constructor %make-content-style))
  (foreground-color nil :type (or null color))
  (background-color nil :type (or null color))
  (attributes nil :type list))

(defun make-content-style (&key foreground-color background-color attributes)
  "Construct a `content-style`. ATTRIBUTES are normalized to canonical order."
  (%make-content-style :foreground-color foreground-color
                       :background-color background-color
                       :attributes (normalize-attributes attributes)))

(defun content-style-default-p (style)
  "True when STYLE has no foreground, background, or attributes."
  (and (null (content-style-foreground-color style))
       (null (content-style-background-color style))
       (null (content-style-attributes style))))

(defun content-style= (a b)
  "Structural equality for content styles."
  (and (let ((fa (content-style-foreground-color a))
             (fb (content-style-foreground-color b)))
         (if (and fa fb) (color= fa fb) (eq fa fb)))
       (let ((ba (content-style-background-color a))
             (bb (content-style-background-color b)))
         (if (and ba bb) (color= ba bb) (eq ba bb)))
       (attribute-set= (content-style-attributes a) (content-style-attributes b))))

(defun copy-content-style* (style)
  "Return a fresh copy of STYLE with a fresh attributes list."
  (%make-content-style
   :foreground-color (content-style-foreground-color style)
   :background-color (content-style-background-color style)
   :attributes (copy-list (content-style-attributes style))))

;;; StyledContent -------------------------------------------------------------

(defstruct (styled-content (:constructor make-styled-content (style content)))
  (style (make-content-style) :type content-style)
  (content "" :type string))

;;; Stylize helpers -----------------------------------------------------------
;;;
;;; These mirror crossterm's `Stylize` trait. Each accepts either a plain string
;;; or an existing `styled-content` and returns a `styled-content`, so they can
;;; be chained, e.g. (on-yellow (red "hi")).

(defun %coerce-styled (x)
  "Coerce X (string or styled-content) into a fresh styled-content."
  (etypecase x
    (styled-content (make-styled-content (copy-content-style* (styled-content-style x))
                                         (styled-content-content x)))
    (string (make-styled-content (make-content-style) x))))

(defun %set-fg (x color)
  (let ((sc (%coerce-styled x)))
    (setf (content-style-foreground-color (styled-content-style sc)) color)
    sc))

(defun %set-bg (x color)
  (let ((sc (%coerce-styled x)))
    (setf (content-style-background-color (styled-content-style sc)) color)
    sc))

(defun %add-attr (x attr)
  (let ((sc (%coerce-styled x)))
    (setf (content-style-attributes (styled-content-style sc))
          (normalize-attributes (cons attr (content-style-attributes
                                            (styled-content-style sc)))))
    sc))

(defun stylize (x &key foreground-color background-color attributes)
  "Apply the given style options to X, returning a styled-content."
  (let ((sc (%coerce-styled x)))
    (when foreground-color
      (setf (content-style-foreground-color (styled-content-style sc)) foreground-color))
    (when background-color
      (setf (content-style-background-color (styled-content-style sc)) background-color))
    (when attributes
      (setf (content-style-attributes (styled-content-style sc))
            (normalize-attributes (append attributes
                                          (content-style-attributes
                                           (styled-content-style sc))))))
    sc))

(defun with-style (style content)
  "Build a styled-content pairing a copy of STYLE with the string CONTENT."
  (make-styled-content (copy-content-style* style) content))

(macrolet ((define-color-helpers (&rest colors)
             `(progn
                ,@(loop for kw in colors
                        for name = (string-downcase (symbol-name kw))
                        for fg = (intern (string-upcase name))
                        for bg = (intern (string-upcase (concatenate 'string "ON-" name)))
                        append `((defun ,fg (x) (%set-fg x ,kw))
                                 (defun ,bg (x) (%set-bg x ,kw)))))))
  (define-color-helpers :black :dark-grey :red :dark-red :green :dark-green
    :yellow :dark-yellow :blue :dark-blue :magenta :dark-magenta
    :cyan :dark-cyan :white :grey))

(defun bold (x) (%add-attr x :bold))
(defun dim (x) (%add-attr x :dim))
(defun italic (x) (%add-attr x :italic))
(defun underlined (x) (%add-attr x :underlined))
(defun reverse-video (x) (%add-attr x :reverse))
(defun crossed-out (x) (%add-attr x :crossed-out))

;;; Unicode column width ------------------------------------------------------
;;;
;;; A pragmatic wcwidth-style implementation replacing termwiz's
;;; `unicode_column_width`. Combining/zero-width code points are width 0, East
;;; Asian Wide/Fullwidth and emoji code points are width 2, everything else 1.

(defun %in-range-p (code ranges)
  "True when CODE falls in any (LO . HI) pair of RANGES."
  (loop for (lo . hi) in ranges
        thereis (and (>= code lo) (<= code hi))))

(defparameter *zero-width-ranges*
  '((#x0300 . #x036F) (#x0483 . #x0489) (#x0591 . #x05BD) (#x0610 . #x061A)
    (#x064B . #x065F) (#x0670 . #x0670) (#x06D6 . #x06DC) (#x0E31 . #x0E31)
    (#x0E34 . #x0E3A) (#x0EB1 . #x0EB1) (#x0EB4 . #x0EB9) (#x200B . #x200F)
    (#x202A . #x202E) (#x2060 . #x2064) (#xFE00 . #xFE0F) (#xFE20 . #xFE2F)
    (#xFEFF . #xFEFF)))

(defparameter *wide-ranges*
  '((#x1100 . #x115F) (#x2329 . #x232A) (#x2E80 . #x303E) (#x3041 . #x33FF)
    (#x3400 . #x4DBF) (#x4E00 . #x9FFF) (#xA000 . #xA4CF) (#xA960 . #xA97F)
    (#xAC00 . #xD7A3) (#xF900 . #xFAFF) (#xFE10 . #xFE19) (#xFE30 . #xFE6F)
    (#xFF00 . #xFF60) (#xFFE0 . #xFFE6) (#x1F300 . #x1FAFF) (#x20000 . #x3FFFD)))

(defun char-column-width (ch)
  "Return the terminal column width (0, 1, or 2) of CH."
  (let ((code (char-code ch)))
    (cond
      ((zerop code) 0)
      ((< code 32) 0)
      ((and (>= code #x7F) (< code #xA0)) 0)
      ((%in-range-p code *zero-width-ranges*) 0)
      ((%in-range-p code *wide-ranges*) 2)
      (t 1))))

(defun string-column-width (string)
  "Sum of the column widths of each character in STRING."
  (loop for ch across string sum (char-column-width ch)))

;;; Grapheme clustering -------------------------------------------------------
;;;
;;; A simplified grapheme segmenter: each base character starts a new cluster,
;;; and following zero-width (combining) characters attach to it. This is enough
;;; for the layout operations superconsole performs.

(defun %grapheme-extends-p (ch)
  "True when CH should attach to the preceding grapheme cluster."
  (zerop (char-column-width ch)))

(defun string-graphemes (string)
  "Split STRING into a list of grapheme-cluster substrings."
  (let ((clusters '())
        (chars '()))
    (flet ((flush ()
             (when chars
               (push (coerce (nreverse chars) 'string) clusters)
               (setf chars '()))))
      (loop for ch across string
            do (if (and chars (%grapheme-extends-p ch))
                   (push ch chars)
                   (progn (flush) (push ch chars))))
      (flush))
    (nreverse clusters)))
