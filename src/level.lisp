(cl:in-package :ball-z-2d)


(defgeneric render (object))

;;;
;;; OBSTACLES
;;;
(defclass obstacle ()
  ((stroke-paint :initform (gamekit:vec4 0 0 0 1) :initarg :stroke-paint :reader stroke-paint-of)))


(defclass line-obstacle (obstacle)
  ((origin :initarg :origin)
   (end :initarg :end)))


(defmethod render ((this line-obstacle))
  (with-slots (origin end) this
    (gamekit:draw-line origin end (stroke-paint-of this) :thickness 5)))


(defclass rect-obstacle (obstacle)
  ((origin :initarg :origin)
   (width :initarg :width)
   (height :initarg :height)))


(defmethod render ((this rect-obstacle))
  (with-slots (origin width height) this
    (gamekit:draw-rect origin width height
                       :stroke-paint (stroke-paint-of this)
                       :thickness 5)))


(defclass ellipse-obstacle (obstacle)
  ((origin :initarg :origin)
   (x-radius :initarg :x-radius)
   (y-radius :initarg :y-radius)))


(defmethod render ((this ellipse-obstacle))
  (with-slots (origin x-radius y-radius) this
    (gamekit:draw-ellipse origin x-radius y-radius
                       :stroke-paint (stroke-paint-of this)
                       :thickness 5)))


(defclass path-obstacle (obstacle)
  ((points :initarg :points)))


(defmethod render ((this path-obstacle))
  (with-slots (points) this
    (gamekit:draw-polyline points (stroke-paint-of this)
                           :thickness 5)))

;;;
;;; LEVEL
;;;
(defclass level ()
  ((obstacles :initarg :obstacles)))


(defmethod render ((this level))
  (with-slots (obstacles) this
    (loop for obstacle in obstacles do
         (render obstacle))))



(defun collect-obstacles (level-descriptor)
  (loop for object in level-descriptor collect
       (alexandria:switch ((getf object :type) :test #'equal)
         ("path" (let ((points (loop for (x y) across (getf object :point-data)
                                  collect (gamekit:vec2 x (- *viewport-height* y)))))
                   (make-instance 'path-obstacle :points points)))
         ("rect" (destructuring-bind (&key x y width height &allow-other-keys) object
                   (make-instance 'rect-obstacle
                                  :origin (gamekit:vec2 (parse-number x)
                                                        (- *viewport-height* (parse-number y)))
                                  :width (parse-number width)
                                  :height (parse-number height))))
         ("line" (destructuring-bind (&key x1 y1 x2 y2 &allow-other-keys) object
                   (make-instance 'line-obstacle
                                  :origin (gamekit:vec2 (parse-number x1)
                                                        (- *viewport-height* (parse-number y1)))
                                  :end (gamekit:vec2 (parse-number x2)
                                                     (- *viewport-height* (parse-number y2))))))
         ("ellipse" (destructuring-bind (&key cx cy rx ry &allow-other-keys) object
                      (make-instance 'ellipse-obstacle
                                     :origin (gamekit:vec2 (parse-number cx)
                                                           (- *viewport-height*
                                                              (parse-number cy)))
                                     :x-radius (parse-number rx)
                                     :y-radius (parse-number ry)))))))

(defun load-level (path)
  (make-instance 'level :obstacles (collect-obstacles (svgp:parse-svg-file path))))
