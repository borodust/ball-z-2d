(cl:in-package :ball-z-2d)


(defvar *viewport-width* 800)
(defvar *viewport-height* 600)


(defun load-levels ()
  (list (svgp:parse-svg-file (asdf:system-relative-pathname :ball-z-2d
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
  (loop for object in (first *levels*) do
       (alexandria:switch ((getf object :type) :test #'equal)
         ("path" (let ((points (loop for (x y) across (getf object :point-data)
                                  collect (gamekit:vec2 x (- *viewport-height* y)))))
                   (gamekit:draw-polyline points
                                          (gamekit:vec4 0 0 0 1)
                                          :thickness 5)))
         ("rect" (destructuring-bind (&key x y width height &allow-other-keys) object
                   (gamekit:draw-rect (gamekit:vec2 (parse-number x)
                                                    (- *viewport-height* (parse-number y)))
                                      (parse-number width)
                                      (parse-number height)
                                      :stroke-paint (gamekit:vec4 0 0 0 1)
                                      :thickness 5)))
         ("ellipse" (destructuring-bind (&key cx cy rx ry &allow-other-keys) object
                      (gamekit:draw-ellipse (gamekit:vec2 (parse-number cx)
                                                          (- *viewport-height*
                                                             (parse-number cy)))
                                            (parse-number rx)
                                            (parse-number ry)
                                            :stroke-paint (gamekit:vec4 0 0 0 1)
                                            :thickness 5))))))


(defun run ()
  (gamekit:start 'ball-z-2d))
