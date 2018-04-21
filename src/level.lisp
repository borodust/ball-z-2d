(cl:in-package :ball-z-2d)

;;;
;;; OBSTACLES
;;;
(defclass obstacle ()
  ((stroke-paint :initform (gamekit:vec4 0 0 0 1) :initarg :stroke-paint :reader stroke-paint-of)
   (shape :initarg :shape)))


(defgeneric make-obstacle-shape (obstacle &key &allow-other-keys))


(defmethod initialize-instance ((this obstacle) &rest args &key &allow-other-keys)
  (apply #'call-next-method this :shape (apply #'make-obstacle-shape this args) args))


;;;
;;; LINE
;;;
(defclass line-obstacle (obstacle)
  ((origin :initarg :origin)
   (end :initarg :end)))


(defmethod make-obstacle-shape ((this line-obstacle) &key origin end)
  (ge.phy:make-segment-shape *universe*
                             (ge.ng:mult origin *unit-scale*)
                             (ge.ng:mult end *unit-scale*)))


(defmethod render ((this line-obstacle))
  (with-slots (origin end) this
    (gamekit:draw-line origin end (stroke-paint-of this) :thickness 5)))


;;;
;;; RECTANGLE
;;;

(defclass rect-obstacle (obstacle)
  ((origin :initarg :origin)
   (width :initarg :width)
   (height :initarg :height)))


(defmethod make-obstacle-shape ((this rect-obstacle) &key origin width height)
  (ge.phy:make-box-shape *universe*
                         (/ width *unit-scale*)
                         (/ height *unit-scale*)
                         :offset (ge.ng:mult origin *unit-scale*)))


(defmethod render ((this rect-obstacle))
  (with-slots (origin width height) this
    (gamekit:draw-rect origin width height
                       :stroke-paint (stroke-paint-of this)
                       :thickness 5)))

;;;
;;; ELLIPSE
;;;
(defclass ellipse-obstacle (obstacle)
  ((origin :initarg :origin)
   (x-radius :initarg :x-radius)
   (y-radius :initarg :y-radius)
   (points :initarg :points)))


(defmethod make-obstacle-shape ((this ellipse-obstacle) &key points)
  (ge.phy:make-polygon-shape *universe* (mapcar #'mult-by-unit points)))


(defmethod render ((this ellipse-obstacle))
  (with-slots (origin x-radius y-radius) this
    (gamekit:draw-ellipse origin x-radius y-radius
                       :stroke-paint (stroke-paint-of this)
                       :thickness 5)))

;;;
;;; PATH
;;;
(defclass path-obstacle (obstacle)
  ((points :initarg :points)))


(defmethod make-obstacle-shape ((this path-obstacle) &key points)
  (ge.phy:make-polyline-shape *universe* (mapcar #'mult-by-unit points)))


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


(defun extract-points (object)
  (loop for (x y) across (getf object :point-data)
        collect (gamekit:vec2 x (- *viewport-height* y))))


(defun collect-obstacles (level-descriptor)
  (loop for object in level-descriptor collect
       (alexandria:switch ((getf object :type) :test #'equal)
         ("path" (make-instance 'path-obstacle :points (extract-points object)))
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
                                     :points (extract-points object)
                                     :x-radius (parse-number rx)
                                     :y-radius (parse-number ry)))))))

(defun load-level (path)
  (make-instance 'level :obstacles (collect-obstacles (svgp:parse-svg-file path))))
