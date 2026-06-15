;;;; examples/finalization.lisp
;;;;
;;;; Port of superconsole's examples/finalization.rs: demonstrates how a component
;;;; can draw different content in `:normal` mode (a live greeting) versus `:final`
;;;; mode (a farewell summary printed once when the console is finalized).
;;;;
;;;; Run with:  sbcl --script examples/finalization.lisp

(require :asdf)
(handler-case (asdf:load-system :supercons)
  (error ()
    (asdf:load-asd
     (truename (merge-pathnames
                (make-pathname :directory '(:relative :up)
                               :name "supercons" :type "asd")
                (or *load-truename* *load-pathname*))))
    (asdf:load-system :supercons)))

(defpackage #:supercons-example/finalization
  (:use #:cl)
  (:local-nicknames (#:sc #:supercons)))

(in-package #:supercons-example/finalization)

(defun demo-console ()
  (or (sc:make-superconsole)
      (progn
        (format *error-output* "~&[not a TTY: using forced 80x24 output]~%")
        (sc:make-superconsole-forced (sc:make-dimensions 80 24)))))

;;; A component representing a store greeter, with distinct final output.
(defclass greeter (sc:component)
  ((name :initarg :name :reader greeter-name)
   (store :initarg :store :reader greeter-store)
   (customers :initarg :customers :reader greeter-customers)
   (correct-num :initarg :correct-num :reader greeter-correct-num)))

(defmethod sc:draw-unchecked ((g greeter) dimensions mode)
  (declare (ignore dimensions))
  (ecase mode
    (:normal
     ;; Print a greeting to each current customer.
     (let ((messages
             (list (sc:line-from-strings
                    (list (format nil "Hello my name is ~a!" (greeter-name g)))))))
       (dolist (customer (greeter-customers g))
         (push (sc:line-from-strings
                (list (format nil "Welcome to ~a, ~a!" (greeter-store g) customer)))
               messages))
       (sc:make-lines (nreverse messages))))
    (:final
     ;; Print a summary about the employee as they leave for the day.
     (sc:make-lines
      (list (sc:line-from-strings
             (list (format nil "~a is leaving ~a"
                           (greeter-name g) (greeter-store g))))
            (sc:line-from-strings
             (list (format nil "~a greeted ~a customers today"
                           (greeter-name g) (greeter-correct-num g)))))))))

(defparameter *people*
  #("Joseph" "Janet" "Bob" "Christie" "Raj"
    "Sasha" "Rayna" "Veronika" "Russel" "David"))

(defparameter *stores*
  #("Target" "Target" "Target" "TJ" "TJ"
    "Walmart" "Wendys" "Wendys" "Uwajimaya" "DSW"))

(defun main ()
  (let ((console (demo-console))
        (name "Alex")
        (last nil))
    (dotimes (i (length *stores*))
      (let* ((store (aref *stores* i))
             (customers (loop for x from i below (min 10 (+ i 2))
                              collect (aref *people* x)))
             (correct-num (1+ i)))
        (sc:superconsole-emit
         console
         (sc:make-lines (list (sc:line-from-strings (list (princ-to-string i))))))
        (sc:superconsole-render
         console (make-instance 'greeter :name name :store store
                                         :customers customers
                                         :correct-num correct-num))
        (setf last (list store customers correct-num))
        (sleep 0.5)))
    (destructuring-bind (store customers correct-num) last
      (sc:superconsole-finalize
       console (make-instance 'greeter :name name :store store
                                       :customers customers
                                       :correct-num correct-num)))))

(main)
