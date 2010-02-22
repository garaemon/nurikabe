;;================================================
;; bar-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurikabe)
(alexandria:define-constant +slider-one-move+ 0.1)
(alexandria:define-constant +slider-continuous-time-thr+ 1.0) ;in sec

(defclass* <slide-button-widget>
    (<button-widget>)
  ((direction (nh:double-vector 1 0))
   (button-callback nil)
   ))

(defclass* <slide-slider-widget>
    (<image-widget>)
  ((verticalp nil)
   (slider-width nil)
   (whole-width 0)
   (slider-position 0.0d0)              ;between 0.0 and 1.0
   (clicked-position nil)               ;to memorize clicked position
   (clicked-region nil)                 ;to memorize clicked region
   (update-callback nil)
   ))

(defclass* <slide-widget>
    (<container-widget>)
  ((position 0.0)
   (whole-width 100)
   (forward-button-widget nil)
   (backward-button-widget nil)
   (slider-widget nil)
   (verticalp nil)
   (update-callback nil))
  )

;; slide button widget
;;
;; +---+
;; | < |  <-- direction
;; +---+
(defmethod initialize-instance :after ((widget <slide-button-widget>)
                                       &rest initargs)
  (declare (ignore initargs))
  (unless (button-region-of widget)
    (setf (button-region-of widget)
          (list 0 0 (width-of widget) (height-of widget))))
  widget)

(defmethod render-button ((widget <slide-button-widget>))
  (clear-image (image-of widget))
  (labels ((ratio->pixel (v)
             (list (round (* (width-of widget) (car v)))
                   (round (* (height-of widget) (cadr v))))))
    (let ((a (list 0.2 0.2))
          (b (list 0.8 0.5))
          (c (list 0.2 0.8)))
      (let* ((direction (direction-of widget))
             (theta (atan (aref direction 1) (aref direction 0))))
        (draw-triangle (image-of widget)
                       (ratio->pixel a)
                       (ratio->pixel b)
                       (ratio->pixel c)
                       :angle theta
                       :color
                       (if (eq (button-state-of widget) :pressed)
                           :grey1 :gray40)
                       :rotate-center (list (/ (width-of widget) 2)
                                            (/ (height-of widget) 2))
                       :fill t))))
  widget)

(defmethod render-widget ((widget <slide-button-widget>))
  (render-button widget)
  (call-next-method))

(defmethod button-press-callback ((widget <slide-button-widget>) x y)
  (setf (button-state-of widget) :pressed)
  (when (button-callback-of widget)
    (funcall (button-callback-of widget)))
  (render-widget widget)
  widget)

(defmethod button-release-callback ((widget <slide-button-widget>) x y)
  (setf (button-state-of widget) :released)
  (render-widget widget)
  widget)

(defmethod leave-notify-callback ((widget <slide-button-widget>))
  (setf (button-state-of widget) :released)
  (render-widget widget)
  widget)

(defmethod nop-callback ((widget <slide-button-widget>))
  (when (eq (button-state-of widget) :pressed)
    ))

;; slider
;;         slider-width
;;          +--------+
;; +--------+--------+--------------------+
;; |        |   +    |                    |
;; +--------+--------+--------------------+
;;        slider-position
(defmethod slider-position->bottom-position ((widget <slide-slider-widget>))
  (with-slots (verticalp width height slider-width slider-position) widget
    (if verticalp
        (round (* (- height slider-width) slider-position))
        (round (* (- width slider-width) slider-position)))))

(defmethod slider-position->upper-position ((widget <slide-slider-widget>))
  (+ (slider-width-of widget) (slider-position->bottom-position widget)))

(defmethod slider-direction-width ((widget <slide-slider-widget>))
  (if (verticalp-of widget) (height-of widget) (width-of widget)))

(defmethod initialize-instance :after ((widget <slide-slider-widget>)
                                       &rest initargs)
  (declare (ignore initargs))
  (unless (slider-width-of widget)
    (let ((r (/ (slider-direction-width widget)
                (float (whole-width-of widget)))))
      (setf (slider-width-of widget) (* r (slider-direction-width widget)))))
  widget)

(defmethod render-slider ((widget <slide-slider-widget>))
  (with-slots (image width height verticalp) widget
    (clear-image image)
    (draw-rectangle image 0 0 width height
                    :color :gray90 :fill t)
    (if verticalp
        (draw-rectangle image
                        0 (slider-position->bottom-position widget)
                        (width-of widget)
                        (slider-width-of widget)
                        :color :gray30 :fill t)
        (draw-rectangle image
                        (slider-position->bottom-position widget) 0
                        (slider-width-of widget)
                        (height-of widget)
                        :color :gray30 :fill t))
    ))

(defmethod render-widget ((widget <slide-slider-widget>))
  (render-slider widget)
  (call-next-method))

(defmethod slider-region ((widget <slide-slider-widget>) x y)
  (with-slots (verticalp) widget
    (let ((target (if verticalp y x)))
      (cond ((< target (slider-position->bottom-position widget))
             :min)
            ((> target (slider-position->upper-position widget))
             :max)
            (t :inside)))))

(defmethod button-press-callback ((widget <slide-slider-widget>) x y)
  (case (slider-region widget x y)
    (:min (min-region-button-press-callback widget x y))
    (:max (max-region-button-press-callback widget x y))
    (:inside (inside-region-button-press-callback widget x y))
    (t (error "unkown region")))
  (render-widget widget)
  t)

(defmethod button-release-callback ((widget <slide-slider-widget>) x y)
  (setf (clicked-region-of widget) nil))

(defmethod resize-callback ((widget <slide-slider-widget>) rw rh)
  (log-format widget "resize-callback is called at ~A with ~A ~A" widget
              rw rh)
  (with-slots (verticalp width height) widget
    ;; update width and height
    (let ((new-width (if verticalp width (round (* rw width))))
          (new-height (if verticalp (round (* rh height)) height)))
      (setf width new-width height new-height)
      ;; update slide-width
      (let ((r (/ (slider-direction-width widget)
                  (float (whole-width-of widget)))))
        (setf (slider-width-of widget) (* r (slider-direction-width widget))))
      (resize widget width height)
      ;; update ximage and so on
      (rebuild-image widget)
      widget)))

(defmethod motion-notify-callback ((widget <slide-slider-widget>) x y code)
  (when (eq (clicked-region-of widget) :inside)
    ;; only enable when inside clicking
    (let ((val (if (verticalp-of widget) y x)))
      (move-slider widget
                   (slider-position-pixel->slider-position-ration
                    widget
                    (- val (if (verticalp-of widget)
                               (cadr (clicked-position-of widget))
                               (car (clicked-position-of widget))))))
      (render-widget widget))))

(defmethod slider-position-pixel->slider-position-ration
    ((widget <slide-slider-widget>) pos)
  (with-slots (verticalp width height slider-width) widget
    (/ pos (float (- (if verticalp height width) slider-width)))))

(defmethod slider-width->slider-position-ration
    ((widget <slide-slider-widget>))
  (with-slots (verticalp width height slider-width) widget
    (/ slider-width (float (- (if verticalp height width) slider-width)))))

(defmethod move-slider ((widget <slide-slider-widget>) v &key (relativep nil))
  (with-slots (slider-position update-callback) widget
    (if relativep (setq v (+ v slider-position)))
    (setf slider-position (cond ((> v 1.0) 1.0)
                                ((< v 0.0) 0.0)
                                (t v)))
    (if update-callback (funcall update-callback))
    slider-position))

(defmethod min-region-button-press-callback ((widget <slide-slider-widget>)
                                             x y)
  (setf (clicked-region-of widget) :min)
  (setf (clicked-position-of widget) (list x y))
  (move-slider widget (- (slider-width->slider-position-ration widget))
               :relativep t))

(defmethod max-region-button-press-callback ((widget <slide-slider-widget>)
                                             x y)
  (setf (clicked-region-of widget) :max)
  (setf (clicked-position-of widget) (list x y))
  (move-slider widget (slider-width->slider-position-ration widget)
               :relativep t))

(defmethod inside-region-button-press-callback ((widget <slide-slider-widget>)
                                                x y)
  (setf (clicked-region-of widget) :inside)
  ;; when inside, clicked-position is relative position in "slide"
  (setf (clicked-position-of widget)
        (if (verticalp-of widget)
            (list (/ (width-of widget) 2)
                  (- y (slider-position->bottom-position widget)))
            (list (- x (slider-position->bottom-position widget))
                  (/ (height-of widget) 2))))
  ;; wait motion callback
  )

;; bar widget
(defmethod init-widget ((widget <slide-widget>))
  (with-slots (verticalp width height forward-button-widget
               backward-button-widget slider-widget whole-width
               parent)
      widget
    (let ((button-size (if verticalp width height))
          (geo (make-geometry :parent widget
                              :vertical (if verticalp :upper :fix-center)
                              :horizontal (if verticalp :fix-center :upper))))
      (unless backward-button-widget
        (setf backward-button-widget
              (make-widget '<slide-button-widget>
                           :lock nil
                           :wait-expose nil
                           :parent widget
                           :geometry geo
                           :width button-size :height button-size
                           :direction (if verticalp
                                          (nh:double-vector 0 -1)
                                          (nh:double-vector -1 0)))))
      (unless slider-widget
        (setf slider-widget
              (make-widget '<slide-slider-widget>
                           :lock nil
                           :wait-expose nil
                           :parent widget
                           :whole-width whole-width
                           :geometry geo
                           :verticalp verticalp
                           :width (if verticalp
                                      button-size
                                      (- width (* 2 button-size)))
                           :height (if verticalp
                                       (- height (* 2 button-size))
                                       button-size))))
      (unless forward-button-widget
        (setf forward-button-widget
              (make-widget '<slide-button-widget>
                           :lock nil
                           :wait-expose nil
                           :parent widget
                           :geometry geo
                           :width button-size :height button-size
                           :direction (if verticalp
                                          (nh:double-vector 0 1)
                                          (nh:double-vector 1 0)))))
      ;; callback
      (setf (button-callback-of (forward-button-widget-of widget))
            #'(lambda ()
                (one-slider-move widget :direction :positive)
                (sync-to-slider-widget widget)
                (render-widget (slider-widget-of widget))))
      (setf (button-callback-of (backward-button-widget-of widget))
            #'(lambda ()
                (one-slider-move widget :direction :negative)
                (sync-to-slider-widget widget)
                (render-widget (slider-widget-of widget))))
      (setf (update-callback-of (slider-widget-of widget))
            #'(lambda ()
                (sync-from-slider-widget widget)))
      widget)))

(defmethod move-slider ((widget <slide-widget>) v &key (relativep nil))
  (if relativep
      (setq v (+ v (position-of widget))))
  (setf (position-of widget) (cond ((> v 1.0) 1.0)
                                   ((< v 0.0) 0.0)
                                   (t v))))

(defmethod one-slider-move ((widget <slide-widget>) &key (direction :positive))
  (move-slider widget (if (eq direction :positive)
                          +slider-one-move+
                          (- +slider-one-move+))
               :relativep t))

(defmethod sync-to-slider-widget ((widget <slide-widget>))
  (setf (slider-position-of (slider-widget-of widget)) (position-of widget))
  (if (update-callback-of widget)
      (funcall (update-callback-of widget) (position-of widget))))

(defmethod sync-from-slider-widget ((widget <slide-widget>))
  (setf (position-of widget) (slider-position-of (slider-widget-of widget)))
  (if (update-callback-of widget)
      (funcall (update-callback-of widget) (position-of widget))))

(defmethod render-widget ((widget <slide-widget>))
  (render-widgets widget))

(defmethod resize-callback ((widget <slide-widget>) rw rh)
  (resize-callback (slider-widget-of widget) rw rh)
  (with-slots (width height) widget
    (resize widget (round (* rw width)) (round (* rh height))))
  (arrange-widgets (car (geometries-of widget))))
