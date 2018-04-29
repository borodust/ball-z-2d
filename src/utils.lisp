(cl:in-package :ball-z-2d)

(declaim (special *universe*
                  *cursor*))


(defvar *viewport-width* 800)
(defvar *viewport-height* 600)
(defvar *unit-scale* 0.01)
(defvar *viewport-half* (gamekit:vec2 (/ *viewport-width* 2)
                                      (/ *viewport-height* 2)))
(defvar *zero-vec2* (gamekit:vec2 0 0))

(defun mult-by-unit (point)
  (gamekit:mult point *unit-scale*))


(defun div-by-unit (point)
  (gamekit:div point *unit-scale*))


(defgeneric render (object)
  (:method (object) (declare (ignore object))))

(defgeneric act (object)
  (:method (object) (declare (ignore object))))


(defun current-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0d0))


(defparameter *max-vial-power* 0.5)

(defstruct (force-vial
            (:constructor %make-force-vial))
  (timestamp 0d0 :type double-float))


(defun make-force-vial ()
  (%make-force-vial))


(defun absorb-force (force-vial)
  (setf (force-vial-timestamp force-vial) (current-seconds)))


(defun peek-force (force-vial)
  (min (if (/= (force-vial-timestamp force-vial) 0d0)
           (let* ((current-time (current-seconds)))
             (- current-time (force-vial-timestamp force-vial)))
           0d0)
       *max-vial-power*))


(defun release-force (force-vial)
  (let ((force (peek-force force-vial)))
    (setf (force-vial-timestamp force-vial) 0d0)
    force))
