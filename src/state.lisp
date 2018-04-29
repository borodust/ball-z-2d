(cl:in-package :ball-z-2d)

(defgeneric button-pressed (game-state button)
  (:method (game-state button) (declare (ignore game-state button))))

(defgeneric button-released (game-state button)
  (:method (game-state button) (declare (ignore game-state button))))

(defgeneric collide (game-state this-shape that-shape)
  (:method (game-state this-shape that-shape)
    (declare (ignore game-state this-shape that-shape))))


(defclass game-state () ())

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


(defun make-level-state (path)
  (make-instance 'level-state :level (load-level path)))


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
    (ge.ng:dispose (if (eq bawl (ge.phy:shape-substance this-shape))
                 that-shape
                 this-shape))
    (log:info "BOOM! ~A" (ge.phy:shape-body this-shape))))


(defmethod act ((this level-state))
  (with-slots (player current-force) this
    (when (> current-force 0d0)
      (apply-force player current-force)
      (setf current-force 0d0))))


(defmethod render ((this level-state))
  (with-slots (level balls player camera) this
    (let* ((player-pos (gamekit:div (ball-position player) *unit-scale* -1))
           (camera-pos (camera-position camera player-pos)))
      (gamekit:translate-canvas (gamekit:x camera-pos) (gamekit:y camera-pos))
      (render level)
      (loop for ball in balls do
        (render ball))
      (render player))))
