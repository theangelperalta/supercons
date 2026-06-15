;;;; examples/cargo.lisp
;;;;
;;;; Port of superconsole's examples/cargo.rs: a cargo-style build progress bar.
;;;; Each crate is emitted as a "Compiling" log line above a live "Building"
;;;; progress bar, and a green "Finished" line is drawn on finalize.
;;;;
;;;; Run with:  sbcl --script examples/cargo.lisp

(require :asdf)
(handler-case (asdf:load-system :supercons)
  (error ()
    (asdf:load-asd
     (truename (merge-pathnames
                (make-pathname :directory '(:relative :up)
                               :name "supercons" :type "asd")
                (or *load-truename* *load-pathname*))))
    (asdf:load-system :supercons)))

(defpackage #:supercons-example/cargo
  (:use #:cl)
  (:local-nicknames (#:sc #:supercons)))

(in-package #:supercons-example/cargo)

(defun demo-console ()
  (or (sc:make-superconsole)
      (progn
        (format *error-output* "~&[not a TTY: using forced 80x24 output]~%")
        (sc:make-superconsole-forced (sc:make-dimensions 80 24)))))

(defparameter *crates*
  '("regex-syntax" "unicode-segmentation" "pest" "memoffset" "crossbeam-epoch"
    "vtparse" "itertools" "libc" "typenum" "memchr" "log" "phf"
    "crossbeam-utils" "anyhow" "num-traits" "lock_api" "thiserror"
    "crossbeam-channel" "parking_lot" "crossterm")
  "The crates being \"compiled\" (a trimmed copy of cargo's crates.txt).")

(defparameter +width+ (1- (length "=======>                  "))
  "Inner width of the progress bar, matching the Rust example.")

(defun build-bar (amount)
  "An `=`-filled bar of length AMOUNT ending in `>`, right-padded with spaces to
+WIDTH+. Mirrors Rust's `[{:=>amount$}{:padding$}]` formatting."
  (let ((arrow (if (plusp amount)
                   (concatenate 'string
                                (make-string (1- amount) :initial-element #\=) ">")
                   "")))
    (concatenate 'string arrow
                 (make-string (- +width+ amount) :initial-element #\Space))))

(defclass loading-bar (sc:component)
  ((crates :initarg :crates :reader loading-bar-crates)
   (iteration :initarg :iteration :reader loading-bar-iteration)))

(defmethod sc:draw-unchecked ((b loading-bar) dimensions mode)
  (declare (ignore dimensions))
  (let ((line
          (ecase mode
            (:normal
             (let* ((iteration (loading-bar-iteration b))
                    (total (length (loading-bar-crates b)))
                    (percentage (/ iteration (float total)))
                    (amount (ceiling (* percentage +width+)))
                    (building (sc:make-span-styled (sc:bold (sc:cyan "   Building "))))
                    (loading (sc:make-span-unstyled
                              (format nil "[~a] ~a/~a: ..."
                                      (build-bar amount) iteration total))))
               (sc:make-line (list building loading))))
            (:final
             (sc:make-line
              (list (sc:make-span-styled (sc:bold (sc:green "   Finished ")))
                    (sc:make-span-unstyled
                     "dev [unoptimized + debuginfo] target(s) in 14.45s")))))))
    (sc:make-lines (list line))))

(defun main ()
  (let ((console (demo-console))
        (crates *crates*))
    (loop for c in crates
          for i from 0
          do (sc:superconsole-emit
              console
              (sc:make-lines
               (list (sc:make-line
                      (list (sc:make-span-styled (sc:bold (sc:green "  Compiling ")))
                            (sc:make-span-unstyled c))))))
             (sc:superconsole-render
              console (make-instance 'loading-bar :crates crates :iteration i))
             (sleep 0.2))
    (sc:superconsole-finalize
     console (make-instance 'loading-bar :crates crates
                                         :iteration (length crates)))))

(main)
