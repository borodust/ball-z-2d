(cl:in-package :ball-z-2d)


(defgeneric discard-level-feature (feature)
  (:method (feature) (declare (ignore feature))))


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


(defmethod discard-level-feature ((this obstacle))
  (with-slots (shape) this
    (ge.ng:dispose shape)))


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


(defun discard-level (level)
  (with-slots (features) level
    (loop for feature in features
          do (discard-level-feature feature))))


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


(defgeneric infuse-level-feature (name feature-type object level)
  (:method (name feature-type object level)
    (error "Unrecognized level ~A: ~A" feature-type name)))


(defmethod infuse-level-feature ((name (eql :path)) (type (eql :obstacle)) object level)
  (let ((obstacle (make-instance 'path-obstacle
                                 :obstacle-p t
                                 :points (extract-points object))))
    (add-level-feature level obstacle)))


(defmethod infuse-level-feature ((name (eql :rect)) (type (eql :obstacle)) object level)
  (destructuring-bind (&key x y width height &allow-other-keys) object

    (let ((obstacle (make-instance 'rect-obstacle
                                   :obstacle-p t
                                   :origin (gamekit:vec2 (parse-number x)
                                                         (invert-y (parse-number y)))
                                   :width (parse-number width)
                                   :height (parse-number height))))
      (add-level-feature level obstacle))))


(defmethod infuse-level-feature ((name (eql :line)) (type (eql :obstacle)) object level)
  (destructuring-bind (&key x1 y1 x2 y2 &allow-other-keys) object
    (let ((obstacle (make-instance 'line-obstacle
                                   :obstacle-p t
                                   :origin (gamekit:vec2 (parse-number x1)
                                                         (invert-y (parse-number y1)))
                                   :end (gamekit:vec2 (parse-number x2)
                                                      (invert-y (parse-number y2))))))
      (add-level-feature level obstacle))))


(defmethod infuse-level-feature ((name (eql :ellipse)) (type (eql :obstacle)) object level)
  (destructuring-bind (&key cx cy rx ry &allow-other-keys) object
    (let ((obstacle (make-instance 'ellipse-obstacle
                                   :obstacle-p t
                                   :origin (gamekit:vec2 (parse-number cx)
                                                         (invert-y (parse-number cy)))
                                   :points (extract-points object)
                                   :x-radius (parse-number rx)
                                   :y-radius (parse-number ry))))
      (add-level-feature level obstacle ))))


(defmethod infuse-level-feature ((name (eql :circle)) (type (eql :controller)) object level)
  (destructuring-bind (&key cx cy &allow-other-keys) object
    (setf (player-spawn-point-of level) (gamekit:vec2 (parse-number cx)
                                                      (invert-y (parse-number cy))))))


(defun init-level-feature (level object)
  (let ((feature-type (parse-feature-type object))
        (name (alexandria:switch ((getf object :type) :test #'equal)
                ("path" :path)
                ("line" :line)
                ("rect" :rect)
                ("ellipse" :ellipse)
                ("circle" :circle))))
    (infuse-level-feature name feature-type object level)))


(defun init-features (level level-descriptor)
  (loop for object in level-descriptor
        do (init-level-feature level object)))


(defun load-level (string)
  (let ((level (make-instance 'level)))
    (init-features level (svgp:parse-svg-string string))
    level))
