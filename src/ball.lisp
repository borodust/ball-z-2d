(cl:in-package :ball-z-2d)


(defclass ball ()
  (radius
   (body :reader %body-of)
   shape
   (color :reader %color-of)))


(defun ball-position (ball)
  (ge.phy:body-position (%body-of ball)))


(defmethod initialize-instance :after ((this ball) &key universe
                                                     position
                                                     (radius 0.2)
                                                     (color (gamekit:vec4 0 0 0 1)))
  (with-slots (body shape (this-radius radius) (this-color color)) this
    (setf this-color color
          this-radius radius
          body (ge.phy:make-rigid-body universe)
          shape (ge.phy:make-circle-shape universe radius :body body)
          (ge.phy:body-position body) position)
    (ge.phy:infuse-circle-mass body 1 radius)))


(defun spawn-ball (universe position)
  (make-instance 'ball :universe universe :position position :color (gamekit:vec4 0.75 0.25 0.25 1)))


(defmethod render ((this ball))
  (with-slots (radius body color) this
    (gamekit:draw-circle (gamekit:div (ge.phy:body-position body) *unit-scale*)
                         (/ radius *unit-scale*)
                         :fill-paint color)))


(defgeneric apply-force (ball force)
  (:method (ball force)))


(defclass master-bawl (ball)
  ((triangle :initform (list (gamekit:vec2 -2 14)
                             (gamekit:vec2 0 17)
                             (gamekit:vec2 2 14))))
  (:default-initargs :radius 0.1 :color (gamekit:vec4 0 0 0 1)))


(defun spawn-master-bawl (universe position)
  (make-instance 'master-bawl :universe universe :position position))


(defun bawl-direction ()
  (gamekit:normalize (gamekit:subt *cursor* (gamekit:vec2 (/ *viewport-width* 2)
                                                          (/ *viewport-height* 2)))))


(defmethod apply-force ((this master-bawl) force)
  (ge.phy:apply-force (%body-of this) (gamekit:mult (bawl-direction) (* force 10000))))


(defmethod render ((this master-bawl))
  (with-slots (triangle) this
    (call-next-method this)
    (gamekit:with-pushed-canvas ()
      (let* ((triangle-pos (gamekit:div (ge.phy:body-position (%body-of this)) *unit-scale*))
             (direction-vec (bawl-direction))
             (triangle-angle (- (atan (gamekit:y direction-vec) (gamekit:x direction-vec)) (/ pi 2))))
        (gamekit:translate-canvas (gamekit:x triangle-pos) (gamekit:y triangle-pos))
        (gamekit:rotate-canvas triangle-angle)
        (gamekit:draw-polygon triangle :stroke-paint (%color-of this) :thickness 2)))))
