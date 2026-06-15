;;;; tests/line.lisp -- ports of content/line.rs tests.

(in-package #:supercons/tests)


(defun colored (text color) (sc:make-span-colored text color))

(deftest line-words-len
  (let ((normal (sc:make-line (list (colored "test" sc:+black+)
                                    (colored "hello" sc:+blue+)
                                    (colored "world" sc:+black+)))))
    (ok (= 14 (sc:line-len normal)))
    (ok (= 0 (sc:line-len (sc:make-line))))))

(deftest line-pad-right
  (let ((test (sc:make-line (list (colored "test" sc:+dark-blue+)
                                  (colored "ok" sc:+dark-cyan+))))
        (new-test (sc:make-line (list (colored "test" sc:+dark-blue+)
                                      (colored "ok" sc:+dark-cyan+)))))
    (sc:line-push test (sc:make-span-unstyled (spaces 4)))
    (sc:line-pad-right new-test 4)
    (ok (sc:line= test new-test))))

(deftest line-pad-left
  (let ((test (sc:make-line (list (colored "test" sc:+dark-cyan+)
                                  (colored "ok" sc:+cyan+))))
        (new-test (sc:make-line (list (colored "test" sc:+dark-cyan+)
                                      (colored "ok" sc:+cyan+)))))
    (sc:line-push-front test (sc:make-span-unstyled (spaces 4)))
    (sc:line-pad-left new-test 4)
    (ok (sc:line= test new-test))))

(deftest line-truncate-line
  (let ((test (sc:make-line (list (colored "test" sc:+blue+) (colored "ok" sc:+red+))))
        (new-test (sc:make-line (list (colored "test" sc:+blue+) (colored "ok" sc:+red+)))))
    (sc:line-truncate-line test 10)
    (ok (sc:line= test new-test))
    (sc:line-truncate-line new-test 5)
    (ok (sc:line= new-test (sc:make-line (list (colored "test" sc:+blue+)
                                                    (colored "o" sc:+red+)))))
    (sc:line-truncate-line new-test 4)
    (ok (sc:line= new-test (sc:make-line (list (colored "test" sc:+blue+)))))
    (sc:line-truncate-line new-test 0)
    (ok (sc:line= new-test (sc:make-line)))))

(deftest line-trim-ends
  (flet ((ln (&rest s) (sc:line-from-strings s)))
    (let ((test (ln "hello" "cat" "world")))
      (sc:line-trim-ends test 0 15)
      (ok (sc:line= test (ln "hello" "cat" "world"))))
    (let ((test (ln "hello" "cat" "world")))
      (sc:line-trim-ends test 2 10)
      (ok (sc:line= test (ln "llo" "cat" "worl"))))
    (let ((test (ln "hello" "cat" "world")))
      (sc:line-trim-ends test 6 2)
      (ok (sc:line= test (ln "at"))))
    (let ((test (ln "hello" "cat" "world")))
      (sc:line-trim-ends test 9 2)
      (ok (sc:line= test (ln "or"))))))

(deftest line-push-collapses
  (let ((line (sc:make-line)))
    (sc:line-push line (colored "ab" sc:+cyan+))
    (sc:line-push line (colored "c" sc:+cyan+))
    (sc:line-push line (colored "d" sc:+red+))
    (ok (sc:line= line (sc:make-line (list (colored "abc" sc:+cyan+)
                                                (colored "d" sc:+red+)))))))

(deftest line-push-front
  (let ((line (sc:make-line)))
    (sc:line-push-front line (colored "d" sc:+cyan+))
    (sc:line-push-front line (colored "c" sc:+cyan+))
    (sc:line-push-front line (colored "ab" sc:+red+))
    (ok (sc:line= line (sc:make-line (list (colored "ab" sc:+red+)
                                                (colored "cd" sc:+cyan+)))))))

(deftest line-fmt-for-test
  (let ((line (sc:make-line (list (colored "abra" sc:+blue+) (colored "cadabra" sc:+red+)))))
    (ok (string= "<span fg=blue>abra</span><span fg=red>cadabra</span>"
                 (sc:line-fmt-for-test line)))))
