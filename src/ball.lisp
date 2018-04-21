(cl:in-package :ball-z-2d)


(defclass ball ()
  (radius body shape))


(defmethod initialize-instance :after ((this ball) &key universe position)
  (with-slots (body shape radius) this
    (setf radius 0.1
          body (ge.phy:make-rigid-body universe)
          shape (ge.phy:make-circle-shape universe radius :body body)
          (ge.phy:body-position body) position)
    (ge.phy:infuse-circle-mass body 1 radius)))


(defun spawn-ball (universe position)
  (make-instance 'ball :universe universe :position position))


(defmethod render ((this ball))
  (with-slots (radius body) this
    (gamekit:draw-circle (gamekit:div (ge.phy:body-position body) *unit-scale*)
                         (/ radius *unit-scale*)
                         :fill-paint (gamekit:vec4 0 0 0 1))))
