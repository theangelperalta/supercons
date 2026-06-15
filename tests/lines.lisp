;;;; tests/lines.lisp -- ports of content/lines.rs tests.

(in-package #:supercons/tests)


(deftest lines-truncate-lines
  (let ((test (slines '("test" "line") '("another one"))))
    (sc:lines-truncate-lines test 5)
    (ok (sc:lines= test (slines '("test" "l") '("anoth")))))
  (let ((empty (sc:make-lines)))
    (sc:lines-truncate-lines empty 5)
    (ok (sc:lines= empty (sc:make-lines)))))

(deftest lines-max-line-length
  (ok (= 11 (sc:lines-max-line-length
             (sc:make-lines (list (sc:make-line) (sline "test" "line") (sline "another one"))))))
  (ok (= 0 (sc:lines-max-line-length (sc:make-lines)))))

(deftest lines-pad-lines-right
  (let ((test (sc:make-lines (list (sline "test" "line") (sline "another one") (sc:make-line)))))
    (sc:lines-pad-lines-right test 11)
    (ok (sc:lines= test
                        (sc:make-lines (list (sline "test" "line" (spaces 14))
                                             (sline "another one" (spaces 11))
                                             (sline (spaces 22))))))))

(deftest lines-pad-lines-left
  (let ((test (sc:make-lines (list (sline "test" "line") (sline "another one") (sc:make-line)))))
    (sc:lines-pad-lines-left test 11)
    (ok (sc:lines= test
                        (sc:make-lines (list (sline (spaces 11) "test" "line")
                                             (sline (spaces 11) "another one")
                                             (sline (spaces 11))))))))

(deftest lines-pad-lines-bottom
  (let ((test (slines '("test") '("another"))))
    (sc:lines-pad-lines-bottom test 3)
    (ok (sc:lines= test (sc:make-lines (list (sline "test") (sline "another")
                                                  (sc:make-line) (sc:make-line) (sc:make-line)))))))

(deftest lines-pad-lines-top
  (let ((test (slines '("test") '("another"))))
    (sc:lines-pad-lines-top test 3)
    (ok (sc:lines= test (sc:make-lines (list (sc:make-line) (sc:make-line) (sc:make-line)
                                                  (sline "test") (sline "another")))))))

(deftest lines-truncate-lines-bottom
  (let ((test (slines '("test") '("another") '("one more"))))
    (sc:lines-truncate-lines-bottom test 1)
    (ok (sc:lines= test (slines '("test"))))))

(deftest lines-justify
  (let ((test (sc:make-lines (list (sline "test") (sc:make-line) (sline "ok")))))
    (sc:lines-justify test)
    (ok (sc:lines= test (sc:make-lines (list (sline "test") (sline (spaces 4))
                                                  (sline "ok" (spaces 2))))))))

(deftest lines-from-multiline-string
  (let* ((style (sc:make-content-style :foreground-color sc:+red+))
         (mk (lambda (s) (sc:make-line (list (sc:make-span-styled-lossy (sc:with-style style s))))))
         (content (format nil "foo bar~%~%baz~%some other line"))
         (test (sc:lines-from-multiline-string content style))
         (expected (sc:make-lines (list (funcall mk "foo bar") (funcall mk "")
                                        (funcall mk "baz") (funcall mk "some other line")))))
    (ok (sc:lines= test expected))))

(defun esc (fmt &rest args) (apply #'format nil fmt (code-char 27) args))

(deftest lines-colored-from-multiline-string
  (let* ((s (format nil "This is a string~%That has both ~a8 bit blue ~a(in both formats)~%in it,~a as well as ~a256 color blue,~%~aand ~aRGB blue as well.~a It resets to the~%console default at the end.~%It can do ~a~abackground colors, ~aforeground colors,~%~acolored, and ~anormal ~abold,~a and it~%strips out ~ainvalid control sequences"
                    (esc "~c[34m") (esc "~c[38;5;4m") (esc "~c[0m") (esc "~c[38;5;20m")
                    (esc "~c[0m") (esc "~c[38;2;0;0;238m") (esc "~c[0m") (esc "~c[38;2;0;0;238m")
                    (esc "~c[44m") (esc "~c[34m") (esc "~c[1m") (esc "~c[0m")
                    (esc "~c[1m") (esc "~c[22m") (esc "~c[D")))
         (expected (format nil "This is a string~%That has both <span fg=ansi(4)>8 bit blue (in both formats)</span>~%<span fg=ansi(4)>in it,</span> as well as <span fg=ansi(20)>256 color blue,</span>~%and <span fg=rgb(0, 0, 238)>RGB blue as well.</span> It resets to the~%console default at the end.~%It can do <span fg=rgb(0, 0, 238) bg=ansi(4)>background colors, </span><span fg=ansi(4) bg=ansi(4)>foreground colors,</span>~%<span fg=ansi(4) bg=ansi(4) bold>colored, and </span>normal <span bold>bold,</span> and it~%strips out invalid control sequences~%")))
    (ok (string= expected (sc:lines-fmt-for-test (sc:lines-from-colored-multiline-string s))))))

(deftest lines-hyperlink
  (let* ((link (esc "~c]8;;")) (end (esc "~c\\"))
         (input (format nil "This is a ~ahttps://example.com~ahyper~a~a~ahttps://example.com~alink~a~a."
                        link end link end link end link end))
         (got (sc:lines-from-colored-multiline-string input))
         (expected (sc:make-lines
                    (list (sc:make-line
                           (list (sc:make-span-unstyled "This is a ")
                                 (sc:span-with-hyperlink (sc:make-span-unstyled "hyperlink")
                                                         (sc:make-hyperlink "https://example.com"))
                                 (sc:make-span-unstyled ".")))))))
    (ok (sc:lines= got expected))))

(deftest lines-fmt-for-test
  (let ((lines (sc:make-lines (list (sc:line-unstyled "orange")
                                    (sc:make-line (list (sc:make-span-colored "pineapple" sc:+yellow+)))))))
    (ok (string= (format nil "orange~%<span fg=yellow>pineapple</span>~%")
                 (sc:lines-fmt-for-test lines)))))
