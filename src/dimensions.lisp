;;;; dimensions.lisp
;;;;
;;;; Port of dimensions.rs: the `Dimensions` rectangle and the `Direction` axis.

(in-package #:supercons)

;;; Direction -----------------------------------------------------------------
;;;
;;; Represented as keywords :horizontal / :vertical.

(defconstant +direction-horizontal+ :horizontal)
(defconstant +direction-vertical+ :vertical)

(deftype direction () '(member :horizontal :vertical))

;;; Dimensions ----------------------------------------------------------------

(defstruct (dimensions (:constructor make-dimensions (&optional (width 0) (height 0))))
  (width 0 :type (integer 0))
  (height 0 :type (integer 0)))

(defun dimensions= (a b)
  "Structural equality for dimensions."
  (and (= (dimensions-width a) (dimensions-width b))
       (= (dimensions-height a) (dimensions-height b))))

(defun dimensions-dimension (dims direction)
  "Return the size of DIMS along DIRECTION."
  (ecase direction
    (:horizontal (dimensions-width dims))
    (:vertical (dimensions-height dims))))

(defun %mul-truncate (lhs rhs)
  "Multiply integer LHS by float RHS, truncating toward zero (Rust `as usize`)."
  (values (truncate (* (coerce lhs 'double-float) rhs))))

(defun dimensions-multiply (dims multiplicand direction)
  "Scale DIMS along DIRECTION by MULTIPLICAND, truncating to a whole dimension."
  (let ((width (dimensions-width dims))
        (height (dimensions-height dims)))
    (ecase direction
      (:horizontal (make-dimensions (%mul-truncate width multiplicand) height))
      (:vertical (make-dimensions width (%mul-truncate height multiplicand))))))

(defun dimensions-saturating-sub (dims subtractor direction)
  "Subtract SUBTRACTOR from DIMS along DIRECTION, clamping at zero."
  (let ((width (dimensions-width dims))
        (height (dimensions-height dims)))
    (ecase direction
      (:horizontal (make-dimensions (max 0 (- width subtractor)) height))
      (:vertical (make-dimensions width (max 0 (- height subtractor)))))))

(defun dimensions-intersect (a b)
  "Smallest bounding box that fits in both A and B."
  (make-dimensions (min (dimensions-width a) (dimensions-width b))
                   (min (dimensions-height a) (dimensions-height b))))

(defun dimensions-union (a b)
  "Smallest bounding box in which both A and B fit."
  (make-dimensions (max (dimensions-width a) (dimensions-width b))
                   (max (dimensions-height a) (dimensions-height b))))

(defun dimensions-contains-p (outer inner)
  "True when INNER fits within OUTER in both dimensions."
  (and (<= (dimensions-width inner) (dimensions-width outer))
       (<= (dimensions-height inner) (dimensions-height outer))))
