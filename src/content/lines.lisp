;;;; content/lines.lisp
;;;;
;;;; Port of content/lines.rs. `lines` wraps a list of `line`s and provides the
;;;; bulk manipulation helpers (padding, truncation, justification, joining) and
;;;; rendering used by the components and engine. Also includes the ANSI-colored
;;;; multiline string parser (replacing termwiz's escape parser).

(in-package #:supercons)

(defstruct (lines (:constructor %make-lines) (:copier nil))
  ;; Ordered list of line objects.
  (vec '() :type list))

(defun make-lines (&optional list)
  "Create a `lines` from LIST of line objects (copied)."
  (let ((ls (%make-lines)))
    (setf (lines-vec ls) (copy-list list))
    ls))

;;; str::lines-style splitting ------------------------------------------------

(defun split-lines (string)
  "Split STRING the way Rust's str::lines does: on newline, stripping a trailing
carriage return, with no trailing empty element for a terminating newline."
  (let ((result '()) (start 0) (len (length string)))
    (flet ((emit (end)
             (let ((e end))
               (when (and (> e start) (char= (char string (1- e)) #\Return))
                 (decf e))
               (push (subseq string start e) result))))
      (loop for i from 0 below len
            when (char= (char string i) #\Newline)
              do (emit i) (setf start (1+ i)))
      (when (< start len) (emit len)))
    (nreverse result)))

;;; Basic accessors / mutators ------------------------------------------------

(defun lines-len (lines) (length (lines-vec lines)))
(defun lines-empty-p (lines) (null (lines-vec lines)))

(defun lines-push (lines line)
  (setf (lines-vec lines) (append (lines-vec lines) (list line)))
  lines)

(defun lines-extend (lines list)
  (setf (lines-vec lines) (append (lines-vec lines) (copy-list list)))
  lines)

(defun lines-max-line-length (lines)
  "Maximum column width across all lines (0 when empty)."
  (reduce #'max (lines-vec lines) :key #'line-len :initial-value 0))

(defun lines-truncate-lines (lines max-width)
  "Truncate every line to at most MAX-WIDTH columns."
  (dolist (line (lines-vec lines)) (line-truncate-line line max-width))
  lines)

(defun lines-pad-lines-right (lines amount)
  "Pad each line on the right, keeping the block rectangular (longest + AMOUNT)."
  (unless (zerop amount)
    (let ((longest (lines-max-line-length lines)))
      (dolist (line (lines-vec lines))
        (line-pad-right line (+ amount (- longest (line-len line)))))))
  lines)

(defun lines-pad-lines-left (lines amount)
  "Prepend AMOUNT columns of padding to each line (ragged right)."
  (unless (zerop amount)
    (dolist (line (lines-vec lines)) (line-pad-left line amount)))
  lines)

(defun lines-justify (lines)
  "Left-justify by right-padding each line to the longest line's width."
  (let ((longest (lines-max-line-length lines)))
    (dolist (line (lines-vec lines))
      (line-pad-right line (- longest (line-len line)))))
  lines)

(defun lines-set-lines-to-exact-width (lines exact-width)
  "Set every line to exactly EXACT-WIDTH columns."
  (dolist (line (lines-vec lines)) (line-to-exact-width line exact-width))
  lines)

(defun lines-pad-lines-bottom (lines amount)
  "Append AMOUNT empty lines."
  (lines-extend lines (loop repeat amount collect (make-line)))
  lines)

(defun lines-pad-lines-top (lines amount)
  "Prepend AMOUNT empty lines."
  (setf (lines-vec lines)
        (append (loop repeat amount collect (make-line)) (lines-vec lines)))
  lines)

(defun lines-truncate-lines-bottom (lines desired-length)
  "Drop lines past DESIRED-LENGTH from the bottom."
  (let ((vec (lines-vec lines)))
    (setf (lines-vec lines) (subseq vec 0 (min desired-length (length vec)))))
  lines)

(defun lines-set-lines-to-exact-length (lines desired-length)
  "Pad or truncate from the bottom so there are exactly DESIRED-LENGTH lines."
  (let ((len (lines-len lines)))
    (cond ((< len desired-length) (lines-pad-lines-bottom lines (- desired-length len)))
          ((> len desired-length) (lines-truncate-lines-bottom lines desired-length))))
  lines)

(defun lines-shrink-lines-to-dimensions (lines dimensions)
  "Truncate columns and rows that fall outside DIMENSIONS."
  (dolist (line (lines-vec lines))
    (line-truncate-line line (dimensions-width dimensions)))
  (lines-truncate-lines-bottom lines (dimensions-height dimensions)))

(defun lines-dimensions (lines)
  "Bounding `dimensions` of a justified version of LINES."
  (make-dimensions (lines-max-line-length lines) (lines-len lines)))

(defun lines-set-lines-to-exact-dimensions (lines dimensions)
  "Pad/truncate to exactly DIMENSIONS."
  (lines-set-lines-to-exact-length lines (dimensions-height dimensions))
  (lines-set-lines-to-exact-width lines (dimensions-width dimensions))
  lines)

(defun dimension-from-output-truncated (output direction)
  "Size of component OUTPUT along DIRECTION (Rust: Dimensions helper)."
  (ecase direction
    (:horizontal (lines-max-line-length output))
    (:vertical (lines-len output))))

;;; Multiline string constructors ---------------------------------------------

(defun lines-from-multiline-string (multiline-string style)
  "Build `lines` applying STYLE to each line of MULTILINE-STRING (lossy)."
  (make-lines
   (mapcar (lambda (l) (make-line (list (make-span-styled-lossy (with-style style l)))))
           (split-lines multiline-string))))

(defun lines-from-multiline-string-raw (multiline-string style)
  "Like `lines-from-multiline-string` but keeps all whitespace (for emit/aux)."
  (make-lines
   (mapcar (lambda (l) (make-line (list (%make-span l (copy-content-style* style) nil))))
           (split-lines multiline-string))))

;;; Equality / formatting -----------------------------------------------------

(defun lines= (a b)
  "Structural equality for `lines`."
  (let ((va (lines-vec a)) (vb (lines-vec b)))
    (and (= (length va) (length vb)) (every #'line= va vb))))

(defun lines-equal (a b)
  "Number of leading lines that are equal between A and B (Rust: lines_equal)."
  (loop for la in (lines-vec a)
        for lb in (lines-vec b)
        while (line= la lb)
        count t))

(defun lines-fmt-for-test (lines)
  "Render LINES as a debug string, one line per row terminated by a newline."
  (with-output-to-string (out)
    (dolist (line (lines-vec lines))
      (write-string (line-fmt-for-test line) out)
      (write-char #\Newline out))))

;;; Rendering -----------------------------------------------------------------

(defun lines-render-with-limit (lines out limit)
  "Render and drain up to LIMIT lines (NIL = all) to stream OUT.
Returns the remaining limit, or NIL when LIMIT was NIL."
  (let* ((vec (lines-vec lines))
         (output-limit (or limit (length vec)))
         (amt (min output-limit (length vec))))
    (loop for i from 0 below amt
          for line in vec
          do (line-render-with-clear-and-nl line out))
    (setf (lines-vec lines) (nthcdr amt vec))
    (if limit (- output-limit amt) nil)))

(defun lines-render-from-line (lines out start)
  "Render lines from index START onward to stream OUT (no draining)."
  (dolist (line (nthcdr start (lines-vec lines)))
    (line-render-with-clear-and-nl line out)))

(defun lines-render-raw (lines out)
  "Render each line plain (with newline) and clear LINES."
  (dolist (line (lines-vec lines))
    (write-string (line-render line) out)
    (write-char #\Newline out))
  (setf (lines-vec lines) '()))

;;; Horizontal join -----------------------------------------------------------

(defun lines-join-horizontally (blocks)
  "Join a list of `lines` BLOCKS side by side into one `lines`."
  (if (null blocks)
      (make-lines)
      (let ((longest (reduce #'max blocks :key #'lines-len :initial-value 0)))
        (dolist (block blocks)
          (lines-set-lines-to-exact-length block longest)
          (lines-justify block))
        (let ((all (first blocks)))
          (dolist (other (rest blocks))
            (loop for all-line in (lines-vec all)
                  for other-line in (lines-vec other)
                  do (dolist (span (line-spans other-line))
                       (line-push all-line span))))
          all))))

;;; ANSI-colored multiline parser ---------------------------------------------

(defun %split-on-char (string ch)
  "Split STRING on CH, returning a list of substrings (empty substrings kept)."
  (let ((result '()) (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) ch)
            do (push (subseq string start i) result) (setf start (1+ i)))
    (push (subseq string start) result)
    (nreverse result)))

(defun %parse-sgr-params (param-str)
  "Parse a CSI parameter string into a list of integers (empty -> 0)."
  (if (zerop (length param-str))
      '()
      (mapcar (lambda (p) (if (zerop (length p)) 0 (or (parse-integer p :junk-allowed t) 0)))
              (%split-on-char param-str #\;))))

(defun %read-extended-color (params start)
  "Read a 38/48 extended color from PARAMS at index START (pointing at 5 or 2).
Returns (values color consumed-count)."
  (let ((mode (nth start params)))
    (cond
      ((eql mode 5) (values (make-ansi-color (or (nth (1+ start) params) 0)) 2))
      ((eql mode 2) (values (make-rgb-color (or (nth (+ start 1) params) 0)
                                            (or (nth (+ start 2) params) 0)
                                            (or (nth (+ start 3) params) 0))
                            4))
      (t (values nil 0)))))

(defun lines-from-colored-multiline-string (multiline-string)
  "Parse MULTILINE-STRING (which may contain ANSI color/hyperlink escapes) into
`lines`, persisting style state across lines and stripping unknown sequences."
  (let ((fg nil) (bg nil) (attrs '()) (hyperlink nil)
        (buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (spans '()))
    (labels
        ((push-current ()
           (let* ((content (coerce buffer 'string))
                  (style (make-content-style :foreground-color fg
                                             :background-color bg
                                             :attributes attrs))
                  (span (make-span-styled-lossy (make-styled-content style content))))
             (when hyperlink (setf span (span-with-hyperlink span hyperlink)))
             (push span spans)
             (setf (fill-pointer buffer) 0)))
         (apply-sgr (params)
           (let ((ps (if (null params) (list 0) params))
                 (idx 0))
             (loop while (< idx (length ps)) do
               (let ((p (nth idx ps)))
                 (cond
                   ((= p 0) (push-current) (setf fg nil bg nil attrs '()) (incf idx))
                   ((= p 1) (push-current) (setf attrs (list :bold)) (incf idx))
                   ((= p 2) (push-current) (setf attrs (list :dim)) (incf idx))
                   ((= p 22) (push-current) (setf attrs '()) (incf idx))
                   ((<= 30 p 37) (push-current) (setf fg (make-ansi-color (- p 30))) (incf idx))
                   ((= p 38) (multiple-value-bind (color consumed)
                                 (%read-extended-color ps (1+ idx))
                               (push-current) (setf fg color) (incf idx (1+ consumed))))
                   ((= p 39) (push-current) (setf fg nil) (incf idx))
                   ((<= 90 p 97) (push-current) (setf fg (make-ansi-color (+ 8 (- p 90)))) (incf idx))
                   ((<= 40 p 47) (push-current) (setf bg (make-ansi-color (- p 40))) (incf idx))
                   ((= p 48) (multiple-value-bind (color consumed)
                                 (%read-extended-color ps (1+ idx))
                               (push-current) (setf bg color) (incf idx (1+ consumed))))
                   ((= p 49) (push-current) (setf bg nil) (incf idx))
                   ((<= 100 p 107) (push-current) (setf bg (make-ansi-color (+ 8 (- p 100)))) (incf idx))
                   (t (incf idx)))))))
         (handle-osc (body)
           (let ((semi1 (position #\; body)))
             (when (and semi1 (string= (subseq body 0 semi1) "8"))
               (let ((semi2 (position #\; body :start (1+ semi1))))
                 (when semi2
                   (let ((uri (subseq body (1+ semi2))))
                     (push-current)
                     (setf hyperlink (if (zerop (length uri)) nil (make-hyperlink uri)))))))))
         (parse-line (str)
           (let ((i 0) (n (length str)))
             (loop while (< i n) do
               (let ((ch (char str i)))
                 (cond
                   ((char= ch #\Escape)
                    (incf i)
                    (when (< i n)
                      (let ((c2 (char str i)))
                        (cond
                          ((char= c2 #\[)
                           (incf i)
                           (let ((start i))
                             (loop while (and (< i n)
                                              (<= #x30 (char-code (char str i)) #x3F))
                                   do (incf i))
                             (let ((final (when (< i n) (char str i))))
                               (when (< i n) (incf i))
                               (when (eql final #\m)
                                 (apply-sgr (%parse-sgr-params (subseq str start (if final (1- i) i))))))))
                          ((char= c2 #\])
                           (incf i)
                           (let ((start i))
                             (loop while (< i n) do
                               (let ((c (char str i)))
                                 (cond
                                   ((char= c (code-char 7)) (return))
                                   ((and (char= c #\Escape) (< (1+ i) n)
                                         (char= (char str (1+ i)) #\\)) (return))
                                   (t (incf i)))))
                             (let ((body (subseq str start i)))
                               (cond ((and (< i n) (char= (char str i) (code-char 7))) (incf i))
                                     ((and (< i n) (char= (char str i) #\Escape))
                                      (incf i) (when (< i n) (incf i))))
                               (handle-osc body))))
                          (t (incf i))))))
                   (t (when (>= (char-code ch) 32) (vector-push-extend ch buffer))
                      (incf i))))))))
      (let ((result '()))
        (dolist (line-str (split-lines multiline-string))
          (setf spans '())
          (parse-line line-str)
          (push-current)
          (push (make-line (nreverse spans)) result))
        (make-lines (nreverse result))))))
