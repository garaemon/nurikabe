;;================================================
;; click-button-widget.lisp
;;
;;
;; button size is defined with button-size.
;; button-size is a double-float value between 0.0d0 and 1.0d0.
;;   +----------------------------+    +
;;   |                            |    | (1 - button-size) / 2.0d0
;;   |     /----------------\     |    +
;;   |     |     BUTTON     |     |    | button-size 
;;   |     \----------------/     |    +
;;   |                            |    | (1 - button-size) / 2.0d0
;;   +----------------------------+    *
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurikabe)

(defmethod initialize-instance :after ((widget <click-button-widget>)
                                       &rest initargs)
  (declare (ignore initargs))
  ;; decide button region from button-size
  (unless (button-region-of widget)
    (setf (button-region-of widget)
          (list 0 0 (width-of widget) (height-of widget))))
  ;; setup draw-pint
  (unless (string-draw-point-of widget)
    (let* ((font-size (button-font-size-of widget))
           (image (image-of widget))
           (ttf-font-size (pixel-font-size->ttf-font-size
                           font-size (font-loader-of image)))
           (font-height (ttf-font-size->pixel-font-height
                         ttf-font-size (font-loader-of image))))
      (let ((line-num 1)
            (string-length (* font-size (length (display-string-of widget)))))
        (setf (string-draw-point-of widget)
              (list (round (/ (- (width-of widget) string-length) 2.0))
                    (round
                     (/ (- (height-of widget) (* line-num font-height)) 2.0))
                    )))))
  widget)

(defmethod render-button ((widget <click-button-widget>))
  (clear-image (image-of widget))
  (let ((left-up-x (button-left-up-x widget))
        (left-up-y (button-left-up-y widget))
        (right-down-x (button-right-down-x widget))
        (right-down-y (button-right-down-y widget)))
    (draw-rectangle (image-of widget)
                    left-up-x left-up-y
                    (- right-down-x left-up-x)
                    (- right-down-y left-up-y)
                    :color (if (eq (button-state-of widget) :pressed)
                               (pressed-color-of widget)
                               (button-color-of widget))
                    :fill t :round 10)
    ;; draw string center of widget
    (if (display-string-of widget)
        (draw-string (image-of widget)
                     (display-string-of widget)
                     (car (string-draw-point-of widget))
                     (cadr (string-draw-point-of widget))
                     :font-size (button-font-size-of widget)))
    ))

(defmethod render-widget ((widget <click-button-widget>))
  (render-button widget)
  (call-next-method)
  )

(defmethod button-press-callback ((widget <click-button-widget>) x y)
  (log-format widget "button-press-calback is called at ~A ~A of ~A"
              x y widget)
  (setf (button-state-of widget) :pressed)
  (render-widget widget)
  widget)

(defmethod button-release-callback ((widget <click-button-widget>) x y)
  (log-format widget "button-release-calback is called at ~A ~A of ~A"
              x y widget)
  (if (eq (button-state-of widget) :pressed)
      (if (button-callback-of widget)
          (funcall (button-callback-of widget))
          (log-format widget "no button-callback")))
  (setf (button-state-of widget) :released)
  (render-widget widget)
  widget)

(defmethod leave-notify-callback ((widget <click-button-widget>))
  (setf (button-state-of widget) :released)
  (render-widget widget)
  widget)
