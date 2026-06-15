;;;; tests/main.lisp
;;;;
;;;; Shared test helpers. Per-module tests live in the sibling files as Rove
;;;; DEFTESTs in the SUPERCONS/TESTS package.

(in-package #:supercons/tests)

;;; Construction helpers ------------------------------------------------------

(defun sline (&rest strings)
  "A line built from STRINGS (each its own unstyled span; adjacent merge)."
  (sc:line-from-strings strings))

(defun slines (&rest rows)
  "Lines built from ROWS, each ROW a list of strings."
  (sc:make-lines (mapcar #'sc:line-from-strings rows)))

(defun spaces (n)
  "A string of N spaces."
  (make-string n :initial-element #\Space))

(defun sstyled (style content)
  "An unstyled-or-styled span from a content-style STYLE and CONTENT string."
  (sc:make-span-styled (sc:with-style style content)))
