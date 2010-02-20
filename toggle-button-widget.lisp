;;================================================
;; toggle-button-widget.lisp
;;
;; +---------------------+
;; | +---+               |
;; | |   |  hogehogehoge |
;; | +---+               |
;; +---------------------+
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurikabe)

(defclass* <toggle-button-widget>
    (<button-widget>)
  ((display-string "")                  ;
   (display-string-offset nil)          ;
   (button-size nil)                    ;
   (button-font-size 10))              ;
  (:documentation
   "widget class for a toggle button.
    +-------------+
    |<--> offset  |
    | x    hoge   |
    +-------------+
   "))


(defmethod initialize-instance :after ((widget <toggle-button-widget>)
                                       &rest initargs)
  (declare (ignore initargs))
  ;; decide button region from font-size
  (let* ((font-size (button-font-size-of widget))
         (image (image-of widget))
         (ttf-font-size (pixel-font-size->ttf-font-size
                         font-size (font-loader-of image)))
         (font-height (ttf-font-size->pixel-font-height
                       ttf-font-size (font-loader-of image))))
    (print font-height)
    ;; setup button size
    (unless (button-size-of widget)
      (setf (button-size-of widget) font-height))
    ;; setup button region
    (unless (button-region-of widget)
      (let ((button-margin (round (/ (- (height-of widget) font-height) 2))))
        (setf (button-region-of widget)
              (list button-margin button-margin
                    (+ button-margin font-height)
                    (- (height-of widget) button-margin)))))
    widget))

(defmethod draw-check-box ((widget <toggle-button-widget>))
  (with-slots (button-size image button-region) widget
    (draw-rectangle image
                    (car button-region) (cadr button-region)
                    button-size button-size :color :black :fill nil)
    (if (button-state-of widget)
        (draw-line image
                   (car button-region) (cadr button-region)
                   (caddr button-region) (cadddr button-region)
                   :color :black))))

(defmethod draw-display-string ((widget <toggle-button-widget>))
  (with-slots (button-font-size button-size display-string image button-region)
      widget
    (draw-string image display-string
                 (+ button-size (caddr button-region)) (cadr button-region)
                 :font-size button-font-size)))

(defmethod render-widget ((widget <toggle-button-widget>))
  (clear-image (image-of widget))
  (draw-check-box widget)
  (draw-display-string widget)
  (call-next-method))

(defmethod button-press-callback ((widget <toggle-button-widget>) x y)
  (log-format widget "button-press-calback is called at ~A ~A of ~A"
              x y widget)
  (with-slots (button-state button-region) widget
    (if (and (< (car button-region) x (caddr button-region))
             (< (cadr button-region) y (cadddr button-region)))
        (setf button-state (not button-state))))
  (render-widget widget)
  widget)