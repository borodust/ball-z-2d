(cl:in-package :ball-z-2d)


;;;
;;; OBSTACLES
;;;
(defclass obstacle ()
  ((stroke-paint :initform (gamekit:vec4 0 0 0 1) :initarg :stroke-paint :reader stroke-paint-of)
   (shape :initarg :shape)))


(defgeneric make-obstacle-shape (obstacle &key &allow-other-keys))


(defmethod initialize-instance ((this obstacle) &rest args &key (obstacle-p t) &allow-other-keys)
  (apply #'call-next-method this :shape (when obstacle-p
                                          (apply #'make-obstacle-shape this args))
         args))


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
;;;
;;;

;;;
;;; LEVEL
;;;
(defclass level ()
  ((features :initform nil)
   (spawn-point :initform nil :accessor player-spawn-point-of)))


(defun add-level-feature (level feature)
  (with-slots (features) level
    (push feature features)))


(defmethod render ((this level))
  (with-slots (features) this
    (loop for feature in features
          do (render feature))))


(defun invert-y (number)
  (- *viewport-height* number))


(defun extract-points (object)
  (loop for (x y) across (getf object :point-data)
        collect (gamekit:vec2 x (invert-y y))))


(defun parse-feature-type (object)
  (destructuring-bind (&key stroke-dasharray &allow-other-keys) object
    (if (or (null stroke-dasharray) (equalp "null" stroke-dasharray))
        :obstacle
        (let* ((pattern (mapcar #'parse-number (split-sequence:split-sequence #\, stroke-dasharray)))
               (first (first pattern))
               (second (second pattern)))
          (if (and first second)
              (if (<= first second)
                  :controller
                  :background)
              :obstacle)))))


(defun init-level-feature (level object)
  (let ((feature-type (parse-feature-type object)))
    (flet ((%add-level-feature (feature)
             (add-level-feature level feature)))
    (alexandria:switch ((getf object :type) :test #'equal)
      ("path" (%add-level-feature
               (make-instance 'path-obstacle
                              :obstacle-p (eq feature-type :obstacle)
                              :points (extract-points object))))
      ("rect" (destructuring-bind (&key x y width height &allow-other-keys) object
                (%add-level-feature
                 (make-instance 'rect-obstacle
                                :obstacle-p (eq feature-type :obstacle)
                                :origin (gamekit:vec2 (parse-number x)
                                                      (invert-y (parse-number y)))
                                :width (parse-number width)
                                :height (parse-number height)))))
      ("line" (destructuring-bind (&key x1 y1 x2 y2 &allow-other-keys) object
                (%add-level-feature
                 (make-instance 'line-obstacle
                                :obstacle-p (eq feature-type :obstacle)
                                :origin (gamekit:vec2 (parse-number x1)
                                                      (invert-y (parse-number y1)))
                                :end (gamekit:vec2 (parse-number x2)
                                                   (invert-y (parse-number y2)))))))
      ("ellipse" (destructuring-bind (&key cx cy rx ry &allow-other-keys) object
                (%add-level-feature
                   (make-instance 'ellipse-obstacle
                                  :obstacle-p (eq feature-type :obstacle)
                                  :origin (gamekit:vec2 (parse-number cx)
                                                        (invert-y (parse-number cy)))
                                  :points (extract-points object)
                                  :x-radius (parse-number rx)
                                  :y-radius (parse-number ry)))))
      ("circle" (destructuring-bind (&key cx cy &allow-other-keys) object
                  (case feature-type
                    (:controller
                     (setf (player-spawn-point-of level) (gamekit:vec2 (parse-number cx)
                                                                       (invert-y
                                                                        (parse-number cy))))))))))))


(defun init-features (level level-descriptor)
  (loop for object in level-descriptor
        do (init-level-feature level object)))


(defun load-level (path)
  (let ((level (make-instance 'level)))
    (init-features level (svgp:parse-svg-file path))
    level))
