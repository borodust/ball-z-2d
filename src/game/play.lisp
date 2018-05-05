(cl:in-package :ball-z-2d)

;;;
;;; LEVEL
;;;


(defun load-level-descriptor ()
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname :ball-z-2d "assets/levels/level.svg")))


(defvar *level-descriptor-data* (load-level-descriptor))


(defun reload-level ()
  (setf *level-descriptor-data* (load-level-descriptor))
  (transition-to 'level-state))


(defclass level-state (game-state)
  ((player :initform nil)
   (force-vial :initform (make-force-vial))
   (current-force :initform 0d0)
   (balls :initform nil)
   (level :initform nil)
   (start-timestamp :initform (current-seconds))
   (camera :initform (make-instance 'camera))))


(defmethod initialize-instance :after ((this level-state) &key)
  (with-slots (player level balls) this
    (setf level (load-level *level-descriptor-data*)
          player (spawn-master-bawl *universe*
                                    (mult-by-unit (player-spawn-point-of level))))
    (loop for enemy-spawn in (enemy-spawn-points-of level)
          do (push (spawn-ball *universe* (mult-by-unit enemy-spawn)) balls))))


(defmethod discard-state ((this level-state))
  (with-slots (player balls level) this
    (discard-ball player)
    (loop for ball in balls
          do (discard-ball ball))
    (discard-level level)))


(defmethod button-pressed ((this level-state) (button (eql :space)))
  (with-slots (force-vial) this
    (absorb-force force-vial)))


#++(defmethod button-pressed ((this level-state) (button (eql :r)))
  (reload-level))


(defmethod button-released ((this level-state) (button (eql :space)))
  (with-slots (force-vial current-force) this
    (setf current-force (release-force force-vial))))


(defgeneric master-collision-p (this that)
  (:method (this that) (declare (ignore this that)) nil))


(defmethod master-collision-p ((this master-bawl) (that ball))
  that)


(defmethod master-collision-p ((that ball) (this master-bawl))
  that)


(defgeneric control-collision-p (this that)
  (:method (this that) (declare (ignore this that)) nil))


(defmethod control-collision-p ((this master-bawl) (that controller))
  that)

(defmethod control-collision-p ((that controller) (this master-bawl))
  that)


(defun format-time (start-timestamp)
  (let* ((total (floor (- (current-seconds) start-timestamp)))
         (minutes (floor (/ total 60)))
         (seconds (mod total 60)))
    (format nil "~2,'0d:~2,'0d" minutes seconds)))


(defmethod pre-collide ((this level-state) this-shape that-shape)
  (with-slots ((bawl player) start-timestamp) this
    (alexandria:if-let ((controller (control-collision-p (ge.phy:shape-substance this-shape)
                                                           (ge.phy:shape-substance that-shape))))
      (if (and (eq (controller-type controller) :line) (= (controller-id controller) #x0000ff))
          (progn
            (transition-to 'victory-state :timestamp (format-time start-timestamp))

            nil)
          t)
      t)))


(defmethod collide ((this level-state) this-shape that-shape)
  (with-slots ((bawl player)) this
    (alexandria:when-let ((other-ball (master-collision-p (ge.phy:shape-substance this-shape)
                                                          (ge.phy:shape-substance that-shape))))
      (let* ((vel (ge.ng:vector-length
                   (gamekit:add (ge.phy:body-linear-velocity (ball-body bawl))
                                (gamekit:mult (ge.phy:body-linear-velocity (ball-body other-ball))
                                              -1)))))
        (when (and (> vel 3) (not (bawl-dead-p bawl)))
          (repair-bawl bawl)
          (kill-ball other-ball)
          (log:info "BOOM! ~A" vel))))))


(defmethod act ((this level-state))
  (with-slots (player current-force) this
    (degrade-bawl player)
    (when (bawl-dead-p player)
      (transition-to 'end-state))
    (when (> current-force 0d0)
      (apply-force player (* current-force 1.5))
      (setf current-force 0d0))))


(defun draw-hud (level-state)
  (with-slots (force-vial player start-timestamp) level-state
    (gamekit:draw-text (format-time start-timestamp) (gamekit:vec2 370 570))

    (let ((meter-len (* 200 (/ (bawl-health player) *max-bawl-health*))))
      (gamekit:draw-text "PATIENCE" (gamekit:vec2 10 10))
      (gamekit:draw-line (gamekit:vec2 130 18) (gamekit:vec2 (+ 130 meter-len) 18)
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
