(cl:in-package :ball-z-2d)


(gamekit:defgame ball-z-2d ()
  ()
  (:viewport-title "Ball Z: Second Dimension"))


(defun run ()
  (gamekit:start 'ball-z-2d))
