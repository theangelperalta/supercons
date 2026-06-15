;;;; tests/components.lisp -- ports of the components/*.rs tests.

(in-package #:supercons/tests)


;;; A component returning a fixed set of lines (copied so draw may mutate).
(defclass fixed (sc:component) ((out :initarg :out)))
(defmethod sc:draw-unchecked ((c fixed) dimensions mode)
  (declare (ignore dimensions mode))
  (sc:copy-lines (slot-value c 'out)))
(defun fixed (lines) (make-instance 'fixed :out lines))

(defun d (w h) (sc:make-dimensions w h))

;;; echo / blank --------------------------------------------------------------

(deftest echo-empty
  (ok (sc:lines= (sc:make-lines)
                      (sc:draw (sc:make-echo (sc:make-lines)) (d 10 10) :normal))))

(deftest echo-basic
  (let ((out (slines '("Line 1") '("Line 2"))))
    (ok (sc:lines= out (sc:draw (sc:make-echo out) (d 10 10) :final)))))

;;; bounding ------------------------------------------------------------------

(deftest bounding-none
  (let ((msg (slines '("hello world"))))
    (ok (sc:lines= msg (sc:draw (sc:make-bounded (sc:make-echo msg) :max-x 40 :max-y 40)
                                     (d 50 50) :normal)))))

(deftest bounding-bounds
  (let ((msg (slines '("hello world") '("hello world"))))
    (ok (sc:lines= (slines '("he"))
                        (sc:draw (sc:make-bounded (sc:make-echo msg) :max-x 2 :max-y 1)
                                 (d 50 50) :normal)))))

;;; spinner -------------------------------------------------------------------

(deftest spinner-cycles
  (ok (= #x280B (char-code (sc::spinner-current-char (sc:make-spinner 0 (sc:line-sanitized "t"))))))
  (ok (= #x2819 (char-code (sc::spinner-current-char (sc:make-spinner 1 (sc:line-sanitized "t"))))))
  (ok (= #x280B (char-code (sc::spinner-current-char (sc:make-spinner 10 (sc:line-sanitized "t")))))))

(deftest spinner-custom-chars
  (let ((chars (coerce '(#\| #\/ #\- #\\) 'vector)))
    (ok (char= #\| (sc::spinner-current-char (sc:make-spinner 0 (sc:line-sanitized "t") :chars chars))))
    (ok (char= #\/ (sc::spinner-current-char (sc:make-spinner 1 (sc:line-sanitized "t") :chars chars))))))

(deftest spinner-render
  (ok (= 1 (sc:lines-len (sc:draw-unchecked (sc:make-spinner 0 (sc:line-sanitized "Loading..."))
                                            (d 80 24) :normal))))
  (ok (= 1 (sc:lines-len (sc:draw-unchecked (sc:make-spinner 0 (sc:line-sanitized "Done!"))
                                            (d 80 24) :final)))))

;;; draw-vertical / draw-horizontal ------------------------------------------

(deftest draw-vertical
  (let ((dv (sc:make-draw-vertical (d 10 20))))
    (sc:draw-vertical-draw dv (fixed (slines '("foo") '("bar"))) :normal)
    (sc:draw-vertical-draw dv (fixed (slines '("baz") '("qux") '("quux"))) :normal)
    (ok (sc:lines= (slines '("foo") '("bar") '("baz") '("qux") '("quux"))
                        (sc:draw-vertical-finish dv)))))

(deftest draw-horizontal
  (let ((dh (sc:make-draw-horizontal (d 50 10))))
    (sc:draw-horizontal-draw dh (fixed (slines '("quick") '("fox") '("over"))) :normal)
    (sc:draw-horizontal-draw dh (fixed (slines '("brown") '("jumped"))) :normal)
    (ok (sc:lines= (slines '("quickbrown ") '("fox  jumped") '("over       "))
                        (sc:draw-horizontal-finish dh)))))

;;; alignment -----------------------------------------------------------------

(defun aligned-draw (lines h v dims)
  (sc:draw (sc:make-aligned (sc:make-echo lines) :horizontal h :vertical v) dims :normal))

(deftest align-left-unjustified
  (let ((orig (slines '("hello world") '("pretty normal test"))))
    (ok (sc:lines= orig (aligned-draw orig :left :top (d 20 20))))))

(deftest align-left-justified
  (let ((orig (slines '("hello world") '("pretty normal test") '("short"))))
    (ok (sc:lines= (sc:make-lines (list (sline "hello world" (spaces 7))
                                             (sline "pretty normal test")
                                             (sline "short" (spaces 13))))
                        (aligned-draw orig :left-justified :top (d 20 20))))))

(deftest align-center
  (let ((orig (slines '("hello world") '("pretty normal testss") '("shorts"))))
    (ok (sc:lines= (sc:make-lines (list (sline (spaces 4) "hello world" (spaces 5))
                                             (sline "pretty normal testss")
                                             (sline (spaces 7) "shorts" (spaces 7))))
                        (aligned-draw orig :center :top (d 20 20))))))

(deftest align-right
  (let ((orig (slines '("hello world") '("pretty normal testsss") '("shorts"))))
    (ok (sc:lines= (sc:make-lines (list (sline (spaces 9) "hello world")
                                             (sline "pretty normal testss")
                                             (sline (spaces 14) "shorts")))
                        (aligned-draw orig :right :top (d 20 20))))))

(deftest align-row-center
  (let* ((orig (slines '("hello world") '("pretty normal testsss") '("shorts")))
         (got (aligned-draw orig :left :center (d 20 10))))
    (ok (= 10 (sc:lines-len got)))
    (ok (every #'sc:line-empty-p (subseq (sc:lines-vec got) 0 3)))))

(deftest align-bottom
  (let* ((orig (slines '("hello world") '("pretty normal testsss") '("shorts")))
         (got (aligned-draw orig :left :bottom (d 20 10))))
    (ok (= 10 (sc:lines-len got)))
    (ok (every #'sc:line-empty-p (subseq (sc:lines-vec got) 0 7)))))

;;; padding -------------------------------------------------------------------

(deftest pad-left
  (let ((msg (sc:make-lines (list (sline "hello world") (sline "ok") (sc:make-line)))))
    (ok (sc:lines= (sc:make-lines (list (sline (spaces 5) "hello world")
                                             (sline (spaces 5) "ok")
                                             (sline (spaces 5))))
                        (sc:draw (sc:make-padded (sc:make-echo msg) :left 5) (d 20 20) :normal)))))

(deftest pad-truncated
  (let ((msg (sc:make-lines (list (sline "hello world") (sline "ok") (sc:make-line)))))
    (ok (sc:lines=
              (sc:make-lines (list (sline (spaces 5) (spaces 5)) (sline (spaces 5) (spaces 5))
                                   (sline (spaces 5) (spaces 5))
                                   (sline (spaces 5) "he" (spaces 3))
                                   (sline (spaces 5) "ok" (spaces 3))
                                   (sline (spaces 5) (spaces 5)) (sline (spaces 5) (spaces 5))
                                   (sline (spaces 5) (spaces 5))))
              (sc:draw (sc:make-padded (sc:make-echo msg) :left 5 :right 3 :top 3 :bottom 3)
                       (d 10 8) :normal)))))

;;; bordering -----------------------------------------------------------------

(defun border-msg ()
  (sc:make-lines (list (sline "Test") (sline "Longer")
                       (sc:make-line (list (sc:make-span-unstyled "Even Longer")
                                           (sc:make-span-unstyled "ok")))
                       (sc:make-line))))

(deftest border-basic
  (let ((got (sc:draw (sc:make-bordered (sc:make-echo (border-msg))) (d 14 5) :normal)))
    (ok (sc:lines=
              got
              (sc:make-lines (list (sline (make-string 14 :initial-element #\-))
                                   (sline "|" "Test" (spaces 8) "|")
                                   (sline "|" "Longer" (spaces 6) "|")
                                   (sline "|" "Even Longer" "o" "|")
                                   (sline (make-string 14 :initial-element #\-))))))))

(deftest border-complex
  (let ((got (sc:draw (sc:make-bordered (sc:make-echo (border-msg))
                                        :spec (sc:make-bordered-spec :top "@@@" :left nil :bottom "@"))
                      (d 13 7) :normal)))
    (ok (sc:lines=
              got
              (sc:make-lines (list (sline (make-string 13 :initial-element #\@))
                                   (sline (make-string 13 :initial-element #\@))
                                   (sline (make-string 13 :initial-element #\@))
                                   (sline "Test" (spaces 8) "|")
                                   (sline "Longer" (spaces 6) "|")
                                   (sline "Even Longer" "o" "|")
                                   (sline (make-string 13 :initial-element #\@))))))))

(deftest border-multi-width-unicode
  (let ((got (sc:draw (sc:make-bordered (sc:make-echo (slines '("Tested")))
                                        :spec (sc:make-bordered-spec :top "🦶" :left nil
                                                                     :right nil :bottom nil))
                      (d 13 7) :normal)))
    (ok (sc:lines= got (slines '("🦶🦶🦶") '("Tested"))))))

;;; splitting -----------------------------------------------------------------

(defun split-draw (children direction kind dims)
  (sc:draw (sc:make-split (mapcar #'sc:make-echo children) direction kind) dims :normal))

(deftest split-h-adaptive
  (ok (sc:lines= (slines '("test" "xx" " ") '("ok" "so" "yyy") '("sync" "z" "  "))
                      (split-draw (list (slines '("test") '("ok" "so") '("sync"))
                                        (slines '("xx") '("yyy") '("z")))
                                  :horizontal :adaptive (d 10 10)))))

(deftest split-h-equal
  (ok (sc:lines= (sc:make-lines (list (sline "test" (spaces 6) "xx" (spaces 8))
                                           (sline "okso" (spaces 6) "yyy" (spaces 7))
                                           (sline "sync" (spaces 6) "z" (spaces 9))))
                      (split-draw (list (slines '("test") '("ok" "so") '("sync"))
                                        (slines '("xx") '("yyy") '("z")))
                                  :horizontal :equal (d 20 20)))))

(deftest split-h-many-sized
  (let ((m1 (sc:make-lines (list (sc:make-line (list (sc:make-span-unstyled "test")
                                                     (sc:make-span-unstyled "ok")))
                                 (sline "also")))))
    (ok (sc:lines= (sc:make-lines (list (sline "test" "o" "hola" (spaces 6) "way w")
                                             (sline "also" " " (spaces 10) (spaces 5))))
                        (split-draw (list m1 (slines '("hola")) (slines '("way way way way too long")))
                                    :horizontal (list :sized '(0.25 0.5 0.25)) (d 20 20))))))

(deftest split-v-equal
  (let* ((top (slines '("Line 1") '("Line 2222")))
         (bottom (slines '("Line 11") '("Line 12") '("Last line just kiddi")))
         (expected (sc:make-lines (append (copy-list (sc:lines-vec top))
                                          (loop repeat 8 collect (sc:make-line))
                                          (copy-list (sc:lines-vec bottom))
                                          (loop repeat 7 collect (sc:make-line))))))
    (ok (sc:lines= expected (split-draw (list top bottom) :vertical :equal (d 20 20))))))

(deftest split-v-adaptive
  (let* ((top (slines '("Line 1") '("Line 2222")))
         (bottom (slines '("Line 11") '("Line 12") '("Last line just kiddi")))
         (expected (sc:make-lines (append (copy-list (sc:lines-vec top))
                                          (copy-list (sc:lines-vec bottom))))))
    (ok (sc:lines= expected (split-draw (list top bottom) :vertical :adaptive (d 20 20))))))

(deftest split-no-children
  (ok (sc:lines-empty-p (sc:draw (sc:make-split '() :horizontal :equal) (d 20 20) :normal))))

(deftest split-different-ratio-count
  (ok (signals (sc:make-split (list (sc:make-blank) (sc:make-blank) (sc:make-blank))
                              :vertical (list :sized '(0.4 0.4)))
               'error)))
