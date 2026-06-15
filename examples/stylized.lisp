;;;; examples/stylized.lisp
;;;;
;;;; Port of superconsole's examples/stylized.rs: demonstrates stylization. A
;;;; store greeter introduces itself (italic + bold) and welcomes customers, while
;;;; an index styled green-on-black is emitted above the canvas each step.
;;;;
;;;; Run with:  sbcl --script examples/stylized.lisp

(require :asdf)
(handler-case (asdf:load-system :supercons)
  (error ()
    (asdf:load-asd
     (truename (merge-pathnames
                (make-pathname :directory '(:relative :up)
                               :name "supercons" :type "asd")
                (or *load-truename* *load-pathname*))))
    (asdf:load-system :supercons)))

(defpackage #:supercons-example/stylized
  (:use #:cl)
  (:local-nicknames (#:sc #:supercons)))

(in-package #:supercons-example/stylized)

(defun demo-console ()
  (or (sc:make-superconsole)
      (progn
        (format *error-output* "~&[not a TTY: using forced 80x24 output]~%")
        (sc:make-superconsole-forced (sc:make-dimensions 80 24)))))

;;; A component representing a store greeter.
(defclass greeter (sc:component)
  ((name :initarg :name :reader greeter-name)
   (store :initarg :store :reader greeter-store)
   (customers :initarg :customers :reader greeter-customers)))

(defmethod sc:draw-unchecked ((g greeter) dimensions mode)
  (declare (ignore dimensions mode))
  (let ((messages
          (list (sc:make-line
                 (list (sc:make-span-styled (sc:italic "Hello my name is "))
                       (sc:make-span-styled (sc:bold (greeter-name g))))))))
    (dolist (customer (greeter-customers g))
      (push (sc:make-line
             (list (sc:make-span-unstyled
                    (format nil "Welcome to ~a, ~a!" (greeter-store g) customer))))
            messages))
    (sc:make-lines (nreverse messages))))

(defparameter *people*
  #("Joseph" "Janet" "Bob" "Christie" "Raj"
    "Sasha" "Rayna" "Veronika" "Russel" "David"))

(defparameter *stores*
  #("Target" "Target" "Target" "TJ" "TJ"
    "Walmart" "Wendys" "Wendys" "Uwajimaya" "DSW"))

(defun main ()
  (let ((console (demo-console))
        (name "Alex"))
    (dotimes (i (length *stores*))
      (let ((styled (sc:on-black (sc:green (princ-to-string i))))
            (customers (loop for x from i below (min 10 (+ i 2))
                             collect (aref *people* x))))
        (sc:superconsole-emit
         console
         (sc:make-lines (list (sc:make-line (list (sc:make-span-styled styled))))))
        (sc:superconsole-render
         console (make-instance 'greeter :name name
                                         :store (aref *stores* i)
                                         :customers customers))
        (sleep 0.5)))
    ;; View the output before it's collapsed.
    (sleep 1)
    (sc:superconsole-finalize console (sc:make-blank))))

(main)
