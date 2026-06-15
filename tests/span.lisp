;;;; tests/span.lisp -- ports of content/span.rs tests.

(in-package #:supercons/tests)


(defparameter *bad-word*
  (concatenate 'string "i'm really gonna do it" (string #\Newline) "汉字"))

(deftest span-invalid
  (ok (signals (sc:make-span-unstyled *bad-word*) 'sc:span-error)))

(deftest span-invalid-sanitized
  (let ((sanitized (sc:span-sanitized *bad-word*)))
    (ok (string= "i'm really gonna do it汉字" (sc:span-content sanitized)))
    (ok (sc:make-span-unstyled (sc:span-content sanitized)))))

(deftest span-multi-column-character
  (let* ((foot (string (code-char #x1F9B6)))
         (span (sc:make-span-unstyled foot)))
    (ok (= 2 (sc:span-len span)))))

(deftest span-padding-equality
  (let ((lhs (sc:make-span-styled-lossy (sc:red "   ")))
        (rhs (sc:make-span-styled-lossy (sc:yellow "   "))))
    (ng (sc:span= lhs rhs)))
  (let ((lhs (sc:make-span-styled-lossy (sc:on-yellow (sc:red "   "))))
        (rhs (sc:make-span-styled-lossy (sc:on-green (sc:yellow "   ")))))
    (ng (sc:span= lhs rhs))))

(deftest span-inequality
  (ng (sc:span= (sc:make-span-styled-lossy (sc:red "hello"))
                      (sc:make-span-styled-lossy (sc:yellow "world")))))

(deftest span-equality
  (ok (sc:span= (sc:make-span-styled-lossy (sc:on-yellow (sc:red "hello")))
                     (sc:make-span-styled-lossy (sc:on-yellow (sc:red "hello"))))))

(deftest span-fmt-for-test
  (let ((span (sstyled (sc:make-content-style :foreground-color sc:+cyan+
                                              :attributes (list :bold :italic))
                       "fish")))
    (ok (string= "<span fg=cyan bold italic>fish</span>"
                 (sc:span-fmt-for-test span)))))
