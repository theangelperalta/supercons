;;;; supercons.asd
;;;;
;;;; ASDF system definition for supercons, a Common Lisp port of the
;;;; `superconsole` Rust crate.

(asdf:defsystem #:supercons
  :description "A Common Lisp port of the superconsole terminal rendering library."
  :author "Ported from superconsole (Meta Platforms, Inc.)"
  :license "MIT OR Apache-2.0"
  :version "0.1.0"
  :depends-on (#:bordeaux-threads #:cffi)
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "error")
     (:file "style")
     (:file "dimensions")
     (:module "content"
      :serial t
      :components ((:file "span")
                   (:file "line")
                   (:file "lines")))
     (:file "component")
     (:module "components"
      :serial t
      :components ((:file "blank")
                   (:file "echo")
                   (:file "bounding")
                   (:file "spinner")
                   (:file "draw-vertical")
                   (:file "draw-horizontal")
                   (:file "alignment")
                   (:file "padding")
                   (:file "bordering")
                   (:file "splitting")))
     (:file "output")
     (:file "superconsole")
     (:file "builder")
     (:file "stdin")
     (:file "testing"))))
  :in-order-to ((test-op (test-op #:supercons/tests))))

(asdf:defsystem #:supercons/tests
  :description "Test suite for supercons."
  :license "MIT OR Apache-2.0"
  :depends-on (#:supercons #:rove)
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "package")
     (:file "main")
     (:file "dimensions")
     (:file "span")
     (:file "line")
     (:file "lines")
     (:file "components")
     (:file "superconsole")
     (:file "output"))))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (uiop:symbol-call :rove :run-suite :supercons/tests)))
