(cl:in-package :ball-z-2d)


(defparameter *camera-speed* 8)


(defclass camera ()
  ((current-position :initform (gamekit:vec2 0 0))
   (timestamp :initform 0d0)))


(defun camera-position (camera player-pos)
  (with-slots (current-position timestamp) camera
    (let* ((cursor-offset (gamekit:div (gamekit:subt *viewport-half* *cursor*) 2))
           (current-seconds (current-seconds))
           (time-delta (- current-seconds timestamp))
           (target-pos (gamekit:add cursor-offset player-pos *viewport-half*))
           (target-vec (gamekit:subt target-pos current-position))
           (target-distance (ge.math:vector-length target-vec)))
      (when (> target-distance 0d0)
        (let ((target-offset (gamekit:mult target-vec
                                           (/ (min (* time-delta *camera-speed* target-distance)
                                                   target-distance)
                                              target-distance))))
          (incf (gamekit:x current-position) (gamekit:x target-offset))
          (incf (gamekit:y current-position) (gamekit:y target-offset))
          (setf timestamp current-seconds)))
      current-position)))
