(cl:in-package :ball-z-2d)

(declaim (special *universe*
                  *cursor*))


(defvar *unit-scale* 0.01)


(defun mult-by-unit (point)
  (gamekit:mult point *unit-scale*))


(defun div-by-unit (point)
  (gamekit:div point *unit-scale*))


(defgeneric render (object))


(defun current-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0d0))


(defstruct (force-vial
            (:constructor %make-force-vial))
  (timestamp 0d0 :type double-float))


(defun make-force-vial ()
  (%make-force-vial))


(defun absorb-force (force-vial)
  (setf (force-vial-timestamp force-vial) (current-seconds)))


(defun release-force (force-vial)
  (let* ((current-time (current-seconds))
         (force (- current-time (force-vial-timestamp force-vial))))
    (setf (force-vial-timestamp force-vial) current-time)
    force))
