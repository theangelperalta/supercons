;;;; examples/readme.lisp
;;;;
;;;; Port of superconsole's examples/readme.rs: the minimal example -- a
;;;; `Bordered` "Hello world!" component that is rendered once and finalized.
;;;;
;;;; Run with:  sbcl --script examples/readme.lisp

(require :asdf)
(handler-case (asdf:load-system :supercons)
  (error ()
    (asdf:load-asd
     (truename (merge-pathnames
                (make-pathname :directory '(:relative :up)
                               :name "supercons" :type "asd")
                (or *load-truename* *load-pathname*))))
    (asdf:load-system :supercons)))

(defpackage #:supercons-example/readme
  (:use #:cl)
  (:local-nicknames (#:sc #:supercons)))

(in-package #:supercons-example/readme)

(defun demo-console ()
  "A SuperConsole for the current terminal, falling back to forced 80x24 output
when stderr is not a compatible TTY (e.g. when piped)."
  (or (sc:make-superconsole)
      (progn
        (format *error-output* "~&[not a TTY: using forced 80x24 output]~%")
        (sc:make-superconsole-forced (sc:make-dimensions 80 24)))))

;;; A component that always draws a single "Hello world!" line.
(defclass hello-world (sc:component) ())

(defmethod sc:draw-unchecked ((component hello-world) dimensions mode)
  (declare (ignore dimensions mode))
  (sc:make-lines (list (sc:line-from-strings '("Hello world!")))))

(defun main ()
  (let* ((console (demo-console))
         (component (sc:make-bordered (make-instance 'hello-world))))
    (sc:superconsole-render console component)
    (sc:superconsole-finalize console component)))

(main)
