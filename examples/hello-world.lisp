;;;; examples/hello-world.lisp
;;;;
;;;; Port of superconsole's examples/hello_world.rs: a basic example of a custom
;;;; component plus emitting scrolling content above the live canvas. The `foo`
;;;; component redraws the number of elapsed seconds and "Hello world!" each tick,
;;;; while processed "words" are emitted as permanent log lines above it.
;;;;
;;;; Run with:  sbcl --script examples/hello-world.lisp

(require :asdf)
(handler-case (asdf:load-system :supercons)
  (error ()
    (asdf:load-asd
     (truename (merge-pathnames
                (make-pathname :directory '(:relative :up)
                               :name "supercons" :type "asd")
                (or *load-truename* *load-pathname*))))
    (asdf:load-system :supercons)))

(defpackage #:supercons-example/hello-world
  (:use #:cl)
  (:local-nicknames (#:sc #:supercons)))

(in-package #:supercons-example/hello-world)

(defun demo-console ()
  (or (sc:make-superconsole)
      (progn
        (format *error-output* "~&[not a TTY: using forced 80x24 output]~%")
        (sc:make-superconsole-forced (sc:make-dimensions 80 24)))))

;;; Prints the seconds elapsed since it was created at each render loop.
(defclass foo (sc:component)
  ((created :initarg :created :reader foo-created)
   (now :initarg :now :reader foo-now)))

(defmethod sc:draw-unchecked ((component foo) dimensions mode)
  (declare (ignore dimensions))
  (if (eq mode :final)
      (sc:make-lines)
      (let ((elapsed (floor (- (foo-now component) (foo-created component))
                            internal-time-units-per-second)))
        (sc:make-lines
         (list (sc:line-from-strings (list (princ-to-string elapsed)))
               (sc:line-from-strings '("Hello world!")))))))

;;; Generate lines to emit from an arbitrary word:
;;;   line 1: the word, then the word reversed
;;;   line 2: "Some stuff to do with strings"
(defun process-word (word)
  (sc:make-lines
   (list (sc:line-from-strings (list word (reverse word)))
         (sc:line-from-strings '("Some stuff to do with strings")))))

(defun now () (get-internal-real-time))

(defun main ()
  (let ((console (demo-console))
        (created (now)))
    (dotimes (i 6)
      (sc:superconsole-emit console (process-word "hello world"))
      (sc:superconsole-render
       console (make-instance 'foo :created created :now (now)))
      (sleep 0.5))
    (sc:superconsole-finalize
     console (make-instance 'foo :created created :now (now)))))

(main)
