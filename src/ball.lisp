(cl:in-package :ball-z-2d)


(defclass ball ()
  (radius
   (dead-p :initform nil)
   (body :reader %body-of)
   shape
   (color :reader %color-of)))


(defun ball-position (ball)
  (ge.phy:body-position (%body-of ball)))


(defun ball-body (ball)
 (%body-of ball))


(defmethod initialize-instance :after ((this ball) &key universe
                                                     position
                                                     (radius 0.2)
                                                     (color (gamekit:vec4 0 0 0 1)))
  (with-slots (body shape (this-radius radius) (this-color color)) this
    (setf this-color color
          this-radius radius
          body (ge.phy:make-rigid-body universe)
          shape (ge.phy:make-circle-shape universe radius :body body :substance this)
          (ge.phy:body-position body) position)
    (ge.phy:infuse-circle-mass body 1 radius)))


(defun kill-ball (ball)
  (with-slots (dead-p shape) ball
    (ge.ng:dispose shape)
    (setf dead-p t
          shape nil)))


(defun discard-ball (ball)
  (with-slots (body shape) ball
    (when shape
      (ge.ng:dispose shape))
    (ge.ng:dispose body)))


(defun spawn-ball (universe position)
  (make-instance 'ball :universe universe :position position :color (gamekit:vec4 0.75 0.25 0.25 1)))


(defmethod render ((this ball))
  (with-slots (radius body color dead-p) this
    (let ((position (div-by-unit (ge.phy:body-position body))))
      (gamekit:with-pushed-canvas ()
        (gamekit:translate-canvas (gamekit:x position) (gamekit:y position))
        (gamekit:draw-circle *zero-vec2* (/ radius *unit-scale*) :fill-paint color)
        (when dead-p
          (gamekit:draw-text "×‸×" (gamekit:vec2 -18 -5)))))))


(defgeneric apply-force (ball force)
  (:method (ball force)))


(defparameter *max-bawl-health* 100)
(defparameter *health-degradation-speed* 10)
(defparameter *health-restoration-amount* 100)

(defclass master-bawl (ball)
  ((health :initform *max-bawl-health*)
   (health-timestamp :initform (current-seconds))
   (triangle :initform (list (gamekit:vec2 -2 14)
                             (gamekit:vec2 0 17)
                             (gamekit:vec2 2 14))))
  (:default-initargs :radius 0.1 :color (gamekit:vec4 0 0 0 1)))


(defun spawn-master-bawl (universe position)
  (make-instance 'master-bawl :universe universe :position position))


(defun bawl-direction ()
  (gamekit:normalize (gamekit:subt *cursor* (gamekit:vec2 (/ *viewport-width* 2)
                                                          (/ *viewport-height* 2)))))


(defun degrade-bawl (bawl)
  (with-slots (health health-timestamp) bawl
    (let ((current-time (current-seconds)))
      (setf health (max 0 (- health (* *health-degradation-speed*
                                       (- current-time health-timestamp))))
            health-timestamp current-time))
    health))


(defun repair-bawl (bawl)
  (with-slots (health) bawl
    (setf health (min *max-bawl-health* (+ health *health-restoration-amount*)))))


(defun bawl-health (bawl)
  (with-slots (health) bawl
    health))


(defun bawl-dead-p (bawl)
  (with-slots (health) bawl
    (= health 0)))


(defmethod apply-force ((this master-bawl) force)
  (ge.phy:apply-force (%body-of this) (gamekit:mult (bawl-direction) (* force 10000))))


(defmethod render ((this master-bawl))
  (with-slots (triangle) this
    (call-next-method this)
    (gamekit:with-pushed-canvas ()
      (let* ((triangle-pos (div-by-unit (ge.phy:body-position (%body-of this))))
             (direction-vec (bawl-direction))
             (triangle-angle (- (atan (gamekit:y direction-vec) (gamekit:x direction-vec)) (/ pi 2))))
        (gamekit:translate-canvas (gamekit:x triangle-pos) (gamekit:y triangle-pos))
        (gamekit:rotate-canvas triangle-angle)
        (gamekit:draw-polygon triangle :stroke-paint (%color-of this) :thickness 2)))))
