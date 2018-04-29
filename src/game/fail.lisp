(cl:in-package :ball-z-2d)

;;;
;;; END
;;;
(defclass end-state (game-state) ())


(defmethod button-pressed ((this end-state) (button (eql :enter)))
  (transition-to 'level-state))


(defmethod render ((this end-state))
  (with-slots () this
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas 2 2)
      (gamekit:draw-text "YOU FAILED" (gamekit:vec2 140 200))
      (gamekit:draw-text "×‸×" (gamekit:vec2 184 160)))
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas 0.6 0.6)
      (gamekit:draw-text "Master Bawl lost all its patience" (gamekit:vec2 500 400)))

    (gamekit:draw-text "Press ENTER to try better this time" (gamekit:vec2 220 150))))
