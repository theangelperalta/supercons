;;;; tests/superconsole.lisp -- ports of superconsole.rs tests.

(in-package #:supercons/tests)


(defun frames (console) (sc:test-output-frames (sc:test-output console)))
(defun joined-frames (console) (apply #'concatenate 'string (frames console)))
(defun repeat-lines (text n) (sc:make-lines (loop repeat n collect (sline text))))

(deftest sc-small-buffer
  (let* ((console (sc:test-console))
         (msg-count (+ sc::+minimum-emit+ 5)))
    (sc:superconsole-emit console (repeat-lines "line 1" msg-count))
    (sc:superconsole-render-general console (sc:make-echo (repeat-lines "line" msg-count))
                                    :normal (sc:make-dimensions 100 2))
    (ok (= (- msg-count sc::+minimum-emit+)
           (sc:lines-len (sc:superconsole-to-emit console))))))

(deftest sc-huge-buffer
  (let* ((console (sc:test-console))
         (big (make-string 600 :initial-element #\x))
         (lines (sc:make-lines (loop repeat 2000 collect (sline big)))))
    (sc:superconsole-emit console lines)
    (sc:superconsole-render-general console (sc:make-echo (slines '("line")))
                                    :normal (sc:make-dimensions 100 20))
    (ok (sc:lines-empty-p (sc:superconsole-to-emit console)))))

(deftest sc-block-render
  (let ((console (sc:test-console))
        (root (sc:make-echo (slines '("state")))))
    (sc:superconsole-render console root)
    (ok (= 1 (length (frames console))))
    (setf (sc:test-output-should-render (sc:test-output console)) nil)
    (sc:superconsole-render console root)
    (ok (= 1 (length (frames console))))
    (sc:superconsole-emit console (slines '("line 1")))
    (sc:superconsole-render console root)
    (ok (= 1 (length (frames console))))))

(deftest sc-block-lines
  (let ((console (sc:test-console))
        (root (sc:make-echo (slines '("state")))))
    (setf (sc:test-output-should-render (sc:test-output console)) nil)
    (sc:superconsole-emit console (slines '("line 1")))
    (sc:superconsole-render console root)
    (ok (= 0 (length (frames console))))
    (setf (sc:test-output-should-render (sc:test-output console)) t)
    (sc:superconsole-emit console (slines '("line 2")))
    (sc:superconsole-render console root)
    (let ((frame (car (last (frames console)))))
      (ok (sc:frame-contains-p frame "state"))
      (ok (sc:frame-contains-p frame "line 1"))
      (ok (sc:frame-contains-p frame "line 2")))))

(deftest sc-block-finalize
  (let ((console (sc:test-console))
        (root (sc:make-echo (slines '("state")))))
    (setf (sc:test-output-should-render (sc:test-output console)) nil)
    (sc:superconsole-emit console (slines '("line 1")))
    (sc:superconsole-emit console (slines '("line 2")))
    (sc:superconsole-render-with-mode console root :final)
    (let ((frame (car (last (frames console)))))
      (ok (sc:frame-contains-p frame "state"))
      (ok (sc:frame-contains-p frame "line 1"))
      (ok (sc:frame-contains-p frame "line 2")))))

(deftest sc-reuse-buffer
  (let ((console (sc:test-console)))
    (sc:superconsole-render console (sc:make-echo (slines '("http://example.com/ link")
                                                          '("number 1, special 1"))))
    (sc:superconsole-render console (sc:make-echo (slines '("http://example.com/ link")
                                                          '("number 2, special 2"))))
    (sc:superconsole-emit console (slines '("special 3")))
    (sc:superconsole-render console (sc:make-echo (slines '("http://example.com/ link")
                                                          '("number 3"))))
    (sc:superconsole-render console (sc:make-echo (slines '("http://example.com/ link")
                                                          '("special 4") '("number 4"))))
    (let ((fs (frames console)))
      (ok (= 4 (length fs)))
      (loop for f in fs for i from 0 do
        (ok (eq (and (member i '(0 2)) t)
                (sc:frame-contains-p f "http://example.com/")))
        (ok (sc:frame-contains-p f (format nil "number ~a" (1+ i))))
        (ok (sc:frame-contains-p f (format nil "special ~a" (1+ i))))))))

(deftest sc-emit-aux
  (let ((console (sc:test-console))
        (root (sc:make-echo (slines '("state")))))
    (sc:superconsole-emit-aux console (slines '("aux line 1")))
    (sc:superconsole-render console root)
    (let ((frame (joined-frames console)))
      (ok (sc:frame-contains-p frame "state"))
      (ok (sc:frame-contains-p frame (sc:aux-output-with-prefix "aux line 1"))))))

(deftest sc-emit-aux-multiple-lines
  (let ((console (sc:test-console))
        (root (sc:make-echo (slines '("state")))))
    (sc:superconsole-emit-aux console (slines '("aux line 1") '("aux line 2") '("aux line 3")))
    (sc:superconsole-render console root)
    (let ((frame (joined-frames console)))
      (ok (sc:frame-contains-p frame "state"))
      (ok (sc:frame-contains-p frame (sc:aux-output-with-prefix "aux line 1")))
      (ok (sc:frame-contains-p frame "aux line 2"))
      (ok (sc:frame-contains-p frame "aux line 3")))))

(deftest sc-block-aux-lines
  (let ((console (sc:test-console))
        (root (sc:make-echo (slines '("state")))))
    (setf (sc:test-output-should-render (sc:test-output console)) nil)
    (sc:superconsole-emit-aux console (slines '("aux line 1")))
    (sc:superconsole-render console root)
    (ok (= 0 (length (frames console))))
    (setf (sc:test-output-should-render (sc:test-output console)) t)
    (sc:superconsole-emit-aux console (slines '("aux line 2")))
    (sc:superconsole-render console root)
    (let ((frame (joined-frames console)))
      (ok (sc:frame-contains-p frame "state"))
      (ok (sc:frame-contains-p frame (sc:aux-output-with-prefix "aux line 1")))
      (ok (sc:frame-contains-p frame "aux line 2")))))

(deftest sc-emit-and-emit-aux
  (let ((console (sc:test-console))
        (root (sc:make-echo (slines '("state")))))
    (sc:superconsole-emit console (slines '("regular line 1")))
    (sc:superconsole-emit console (slines '("regular line 2")))
    (sc:superconsole-emit-aux console (slines '("aux line 1")))
    (sc:superconsole-emit-aux console (slines '("aux line 2")))
    (sc:superconsole-render console root)
    (let ((frame (joined-frames console)))
      (ok (sc:frame-contains-p frame "state"))
      (ok (sc:frame-contains-p frame "regular line 1"))
      (ok (sc:frame-contains-p frame "regular line 2"))
      (ok (sc:frame-contains-p frame (sc:aux-output-with-prefix "aux line 1")))
      (ok (sc:frame-contains-p frame "aux line 2")))))

(deftest sc-emit-aux-with-aux-incompatible-output
  (let ((console (sc:test-console-aux-incompatible))
        (root (sc:make-echo (slines '("state")))))
    (sc:superconsole-emit-aux console (slines '("aux line 1") '("aux line 2")))
    (sc:superconsole-render console root)
    (let ((frame (joined-frames console)))
      (ok (sc:frame-contains-p frame "state"))
      (ok (sc:frame-contains-p frame (format nil "aux line 1~%aux line 2"))))))

(deftest sc-resize-narrower-clears-wrapped-rows
  ;; A canvas line drawn while the window is wide gets reflowed across several
  ;; physical rows once the window is made narrower. The in-place redraw must
  ;; walk the cursor up by physical rows, not logical lines, or the wrapped
  ;; remainder is orphaned on screen. Regression for the resize "leftover text"
  ;; bug: a 31-column line at width 20 occupies ceil(31/20)=2 rows, so the second
  ;; render's clear must move up 2 rows (ESC[2A), not 1.
  (let ((console (sc:test-console))
        (root (sc:make-echo (slines '("0123456789012345678901234567890")))))
    (sc:superconsole-render-general console root :normal (sc:make-dimensions 40 10))
    (sc:superconsole-render-general console root :normal (sc:make-dimensions 20 10))
    (let ((second (car (last (frames console)))))
      (ok (search (format nil "~c[2A" #\Escape) second))
      (ng (search (format nil "~c[1A" #\Escape) second)))))
