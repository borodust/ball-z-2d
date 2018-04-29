(cl:in-package :ball-z-2d)

;;;
;;; LEVEL
;;;

(defclass start-state (game-state)
  ((first-level-path :initarg :first-level-path :initform (error ":first-level-path missing"))))


(defmethod button-pressed ((this start-state) (button (eql :enter)))
  (with-slots (first-level-path) this
    (transition-to 'level-state :level (load-level first-level-path))))


(defmethod render ((this start-state))
  (with-slots () this
    (gamekit:draw-text "Press ENTER to begin" (gamekit:vec2 10 10))))
