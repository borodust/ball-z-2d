(cl:in-package :ball-z-2d)

;;;
;;; END
;;;
(defclass end-state (game-state) ())


(defmethod button-pressed ((this end-state) (button (eql :enter))))


(defmethod render ((this end-state))
  (with-slots () this
    (gamekit:draw-text "Press ENTER to restart" (gamekit:vec2 10 10))))
