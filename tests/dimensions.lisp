;;;; tests/dimensions.lisp -- ports of dimensions.rs tests.

(in-package #:supercons/tests)


(deftest dimensions-intersect
  (ok (sc:dimensions= (sc:make-dimensions 5 3)
                      (sc:dimensions-intersect (sc:make-dimensions 5 10)
                                               (sc:make-dimensions 8 3)))))

(deftest dimensions-union
  (ok (sc:dimensions= (sc:make-dimensions 8 10)
                      (sc:dimensions-union (sc:make-dimensions 5 10)
                                           (sc:make-dimensions 8 3)))))

(deftest dimensions-contains
  (let ((lhs (sc:make-dimensions 8 10))
        (rhs (sc:make-dimensions 5 3)))
    (ok (sc:dimensions-contains-p lhs rhs))
    (ng (sc:dimensions-contains-p rhs lhs))
    (ok (sc:dimensions-contains-p lhs lhs))))
