;;;; tests/package.lisp

(defpackage #:supercons/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:sc #:supercons))
  (:export #:run-tests))

(in-package #:supercons/tests)

(defun run-tests ()
  (rove:run-suite :supercons/tests))
