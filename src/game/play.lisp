(cl:in-package :ball-z-2d)

;;;
;;; LEVEL
;;;

(defclass level-state (game-state)
  ((player :initform nil)
   (force-vial :initform (make-force-vial))
   (current-force :initform 0d0)
   (balls :initform nil)
   (level :initarg :level :initform nil)
   (camera :initform (make-instance 'camera))))


(defmethod initialize-instance :after ((this level-state) &key)
  (with-slots (player level) this
    (setf player (spawn-master-bawl *universe*
                                    (gamekit:mult (player-spawn-point-of level) *unit-scale*)))))


(defmethod discard-state ((this level-state))
  (with-slots (player balls level) this
    (discard-ball player)
    (loop for ball in balls
          do (discard-ball ball))
    (discard-level level)))


(defmethod button-pressed ((this level-state) (button (eql :mouse-left)))
  (with-slots (balls) this
    (push (spawn-ball *universe*
                      (gamekit:vec2 (* (gamekit:x *cursor*) *unit-scale*)
                                    (* (gamekit:y *cursor*) *unit-scale*)))
          balls)))


(defmethod button-pressed ((this level-state) (button (eql :space)))
  (with-slots (force-vial) this
    (absorb-force force-vial)))


(defmethod button-released ((this level-state) (button (eql :space)))
  (with-slots (force-vial current-force) this
    (setf current-force (release-force force-vial))))


(defgeneric master-collision-p (this that)
  (:method (this that) (declare (ignore this that)) nil))


(defmethod master-collision-p ((this master-bawl) (that ball))
  this)

(defmethod master-collision-p ((that ball) (this master-bawl))
  this)


(defmethod collide ((this level-state) this-shape that-shape)
  (alexandria:when-let ((bawl (master-collision-p (ge.phy:shape-substance this-shape)
                                                  (ge.phy:shape-substance that-shape))))
    (let* ((other-ball (ge.phy:shape-substance
                        (if (eq bawl (ge.phy:shape-substance this-shape))
                            that-shape
                            this-shape)))
           (vel (ge.ng:vector-length
                 (gamekit:add (ge.phy:body-linear-velocity (ball-body bawl))
                              (ge.phy:body-linear-velocity (ball-body other-ball))))))
      (when (and (> vel 9) (not (bawl-dead-p bawl)))
        (repair-bawl bawl)
        (kill-ball other-ball)
        (log:info "BOOM! ~A" vel)))))


(defmethod act ((this level-state))
  (with-slots (player current-force) this
    (degrade-bawl player)
    (when (bawl-dead-p player)
      (transition-to 'end-state))
    (when (> current-force 0d0)
      (apply-force player current-force)
      (setf current-force 0d0))))


(defun draw-hud (level-state)
  (with-slots (force-vial player) level-state
    (gamekit:draw-text "00:00" (gamekit:vec2 370 570))

    (let ((meter-len (* 200 (/ (bawl-health player) *max-bawl-health*))))
      (gamekit:draw-text "HEALTH" (gamekit:vec2 10 10))
      (gamekit:draw-line (gamekit:vec2 105 18) (gamekit:vec2 (+ 105 meter-len) 18)
                         (gamekit:vec4 0 0 0 1) :thickness 18))

    (let ((meter-len (* 190 (/ (peek-force force-vial) *max-vial-power*))))
      (gamekit:draw-text "POWER" (gamekit:vec2 500 10))
      (gamekit:draw-line (gamekit:vec2 590 18) (gamekit:vec2 (+ 600 meter-len) 18)
                         (gamekit:vec4 0 0 0 1) :thickness 18))))



(defmethod render ((this level-state))
  (with-slots (level balls player camera) this
    (gamekit:with-pushed-canvas ()
      (let* ((player-pos (gamekit:div (ball-position player) *unit-scale* -1))
             (camera-pos (camera-position camera player-pos)))
        (gamekit:translate-canvas (gamekit:x camera-pos) (gamekit:y camera-pos))
        (render level)
        (loop for ball in balls do
          (render ball))
        (render player)))
    (draw-hud this)))
