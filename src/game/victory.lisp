(cl:in-package :ball-z-2d)

;;;
;;; END
;;;
(defclass victory-state (game-state)
  ((timestamp :initarg :timestamp)))


(defmethod button-pressed ((this victory-state) (button (eql :enter)))
  (transition-to 'level-state))


(defmethod render ((this victory-state))
  (with-slots (timestamp) this
    (gamekit:draw-text timestamp (gamekit:vec2 370 570))
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas 2 2)
      (gamekit:draw-text "YOU ARE AWESOME" (gamekit:vec2 100 200))
      (gamekit:draw-text "^͜^" (gamekit:vec2 190 160)))
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas 0.6 0.6)
      (gamekit:draw-text "Master Bawl escaped the boredom" (gamekit:vec2 500 400)))
    (gamekit:draw-text "Press ENTER to bring it back" (gamekit:vec2 250 150))))
