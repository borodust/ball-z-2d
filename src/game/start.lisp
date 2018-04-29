(cl:in-package :ball-z-2d)

;;;
;;; LEVEL
;;;

(defclass start-state (game-state) ())


(defmethod button-pressed ((this start-state) (button (eql :enter)))
  (transition-to 'level-state))


(defmethod render ((this start-state))
  (with-slots () this
    (gamekit:draw-text "Press ENTER to begin" (gamekit:vec2 10 10))))
