(cl:in-package :ball-z-2d)

(defgeneric button-pressed (game-state button)
  (:method (game-state button) (declare (ignore game-state button))))

(defgeneric button-released (game-state button)
  (:method (game-state button) (declare (ignore game-state button))))

(defgeneric collide (game-state this-shape that-shape)
  (:method (game-state this-shape that-shape)
    (declare (ignore game-state this-shape that-shape))))


(defclass game-state () ())


(defgeneric discard-state (state)
  (:method (state) (declare (ignore state))))
