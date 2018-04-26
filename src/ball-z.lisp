(cl:in-package :ball-z-2d)


(defparameter *universe-step* 0.014)
(defparameter *step-split* 4)


(gamekit:defgame ball-z-2d ()
  ((universe :initform nil)
   (game-state :initform nil)
   (cursor :initform (gamekit:vec2 0 0)))
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*)
  (:viewport-title "Ball Z: Second Dimension"))


(defmethod initialize-instance ((this ball-z-2d) &rest args &key &allow-other-keys)
  (destructuring-bind (&rest args &key depends-on &allow-other-keys) args
    (apply #'call-next-method this :depends-on (append (list 'ge.phy:physics-system) depends-on)
           args)))


(defmacro with-game-specials ((game) &body body)
  (alexandria:with-gensyms (this-universe this-cursor)
    `(with-slots ((,this-universe universe) (,this-cursor cursor)) ,game
       (let ((*universe* ,this-universe)
             (*cursor* ,this-cursor))
         ,@body))))


(defmethod gamekit:post-initialize ((this ball-z-2d))
  (with-slots (universe cursor game-state) this
    (setf universe (ge.phy:make-universe :2d)
          (ge.phy:gravity universe) (gamekit:vec2 0 -9.81))
    (with-game-specials (this)
      (setf game-state (make-level-state
                        (asdf:system-relative-pathname :ball-z-2d "assets/levels/level.svg"))))
    (gamekit:bind-cursor (lambda (x y)
                           (setf (gamekit:x cursor) x
                                 (gamekit:y cursor) y)))
    (flet ((%bind-button (button)
             (gamekit:bind-button button :pressed
                                  (lambda ()
                                    (with-game-specials (this)
                                      (button-pressed game-state button))))
             (gamekit:bind-button button :released
                                  (lambda ()
                                    (with-game-specials (this)
                                      (button-released game-state button))))))
      (%bind-button :mouse-left)
      (%bind-button :space))))


(defmethod gamekit:act ((this ball-z-2d))
  (with-slots (game-state) this
    (with-game-specials (this)
      (act game-state)
      (loop for i from 0 below *step-split*
            do (ge.phy:observe-universe *universe* (/ *universe-step* *step-split*))))))


(defmethod gamekit:draw ((this ball-z-2d))
  (with-slots (game-state) this
    (with-game-specials (this)
      (render game-state))))


(defun run ()
  (gamekit:start 'ball-z-2d))
