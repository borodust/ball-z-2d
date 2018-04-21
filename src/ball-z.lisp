(cl:in-package :ball-z-2d)


(defvar *viewport-width* 800)
(defvar *viewport-height* 600)


(defun load-levels ()
  (list (load-level (asdf:system-relative-pathname :ball-z-2d
                                                   "assets/levels/level.svg"))))


(defvar *levels* (load-levels))


(gamekit:defgame ball-z-2d ()
  ()
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*)
  (:viewport-title "Ball Z: Second Dimension"))


(defmethod initialize-instance ((this ball-z-2d) &rest args &key &allow-other-keys)
  (destructuring-bind (&rest args &key depends-on &allow-other-keys) args
    (apply #'call-next-method this :depends-on (append (list 'ge.phy:physics-system) depends-on)
           args)))


(defmethod gamekit:draw ((this ball-z-2d))
  (gamekit:translate-canvas 0 -50)
  (render (first *levels*)))


(defun run ()
  (gamekit:start 'ball-z-2d))
