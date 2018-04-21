(cl:in-package :ball-z-2d)

(declaim (special *universe*))


(defvar *unit-scale* 0.01)


(defun mult-by-unit (point)
  (gamekit:mult point *unit-scale*))


(defun div-by-unit (point)
  (gamekit:div point *unit-scale*))


(defgeneric render (object))
