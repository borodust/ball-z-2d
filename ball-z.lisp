(cl:in-package :ball-z-2d)


(gamekit:defgame ball-z-2d ()
  ()
  (:viewport-title "Ball Z: Second Dimension"))


(defmethod initialize-instance ((this ball-z-2d) &rest args &key &allow-other-keys)
  (destructuring-bind (&rest args &key depends-on &allow-other-keys) args
    (apply #'call-next-method this :depends-on (append (list 'ge.phy:physics-system) depends-on)
           args)))


(defun run ()
  (gamekit:start 'ball-z-2d))
