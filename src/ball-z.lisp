(cl:in-package :ball-z-2d)


(defvar *viewport-width* 800)
(defvar *viewport-height* 600)
(defvar *universe-step* 0.014)


(gamekit:defgame ball-z-2d ()
  ((universe)
   (level)
   (balls :initform nil)
   (cursor :initform (gamekit:vec2 0 0)))
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*)
  (:viewport-title "Ball Z: Second Dimension"))


(defmethod initialize-instance ((this ball-z-2d) &rest args &key &allow-other-keys)
  (destructuring-bind (&rest args &key depends-on &allow-other-keys) args
    (apply #'call-next-method this :depends-on (append (list 'ge.phy:physics-system) depends-on)
           args)))


(defmethod gamekit:post-initialize ((this ball-z-2d))
  (with-slots (universe level cursor balls) this
    (setf universe (ge.phy:make-universe :2d)
          (ge.phy:gravity universe) (gamekit:vec2 0 -9.81))
    (let ((*universe* universe))
      (setf level (load-level (asdf:system-relative-pathname
                               :ball-z-2d "assets/levels/level.svg")))
      (gamekit:bind-cursor (lambda (x y)
                             (setf (gamekit:x cursor) x
                                   (gamekit:y cursor) y)))
      (gamekit:bind-button :mouse-left :pressed
                           (lambda ()
                             (push (spawn-ball universe
                                               (gamekit:vec2 (* (gamekit:x cursor) *unit-scale*)
                                                             (* (gamekit:y cursor) *unit-scale*)))
                                   balls))))))


(defmethod gamekit:act ((this ball-z-2d))
  (with-slots (universe) this
    (loop for i from 0 below 3
          do (ge.phy:observe-universe universe (/ *universe-step* 3)))))


(defmethod gamekit:draw ((this ball-z-2d))
  (with-slots (level balls) this
    #++(gamekit:translate-canvas 0 -50)
    (loop for ball in balls do
      (render ball))
    (render level)))


(defun run ()
  (gamekit:start 'ball-z-2d))
