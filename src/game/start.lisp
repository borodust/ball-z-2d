(cl:in-package :ball-z-2d)

;;;
;;; LEVEL
;;;

(defclass start-state (game-state) ())


(defmethod button-pressed ((this start-state) (button (eql :enter)))
  (transition-to 'level-state))


(defmethod render ((this start-state))
  (with-slots () this
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas 2 2)
      (gamekit:draw-text "BALL-Z" (gamekit:vec2 160 200))
      (gamekit:draw-text "Second Dimension" (gamekit:vec2 100 160)))
    (gamekit:draw-text "Press ENTER to begin" (gamekit:vec2 290 150))))
