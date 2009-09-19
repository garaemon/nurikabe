;;================================================
;; click-button-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurikabe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <click-button-widget>
;;
;; /------------\
;;|     hoge     |
;; \------------/
;;
(defmethod make-click-button ((win <window>)
                              display-string
                              &key
                              x
                              y
                              (font-size 0.01)
                              (press-callback #'(lambda () t)))
  (let ((string-bbox (drawn-string-bounding-box (font-loader-of (image-of win))
                                                display-string
                                                0 0
                                                :font-size font-size))
        (button-margin 20))
    (let ((width (+ (* 2 button-margin)
                    (abs (- (cdr (assoc :max-x string-bbox))
                            (cdr (assoc :min-x string-bbox))))))
          (height (abs (- (cdr (assoc :max-y string-bbox))
                          (cdr (assoc :min-y string-bbox))))))
      (let ((widget (make-widget '<click-button-widget>
                                 :x (round x)
                                 :y (round y)
                                 :width (round width)
                                 :height (round height)
                                 :display-string display-string
                                 :display-string-margin button-margin
                                 :parent win
                                 :font-size font-size
                                 :press-callback press-callback)))
        widget))))

(defmethod inside-button-region-p ((widget <click-button-widget>) pos)
  (let ((x (car pos))
        (y (cadr pos)))
    (if (and (< 0 x)
             (< 0 y)
             (> (width-of widget) x)
             (> (height-of widget) y))
        (progn
          (log-format widget
                      "inside of ~A" widget)
          t)
        (progn
          (log-format widget
                      "outside of ~A" widget)
          nil))))

;; callbacks
(defmethod render-widget ((widget <click-button-widget>))
  (let ((string-bbox (drawn-string-bounding-box
                      (font-loader-of (image-of (parent-of widget)))
                      (display-string-of widget)
                      0 0
                      :font-size (font-size-of widget))))
    (if (debugp-of widget)
        (draw-rectangle (image-of widget)
                        0
                        0
                        (width-of widget)
                        (height-of widget)
                        :color :red :fill nil))
    ;; round 20
    (draw-rectangle (image-of widget)
                    0
                    0
                    (width-of widget)
                    (height-of widget)
                    :color (if (button-state-of widget)
                               :blue :skyblue)
                    :fill t
                    :round 5)
    (draw-string (image-of widget)
                 (display-string-of widget)
                 (+ (- (cdr (assoc :min-x string-bbox)))
                    (display-string-margin-of widget))
                 (- (cdr (assoc :min-y string-bbox)))
                 :font-size (font-size-of widget))
    ))

(defmethod button-press-callback ((widget <click-button-widget>) x y)
  (log-format widget
              ":button-press-callback (~A, ~A) is called in <click-button-widget>" x y)
  (when (inside-button-region-p widget (list x y))
    (unless (button-state-of widget)
      ;; nil -> t
      (funcall (press-callback-of widget)))
    (setf (button-state-of widget)
          t))
  t)

(defmethod button-release-callback ((widget <click-button-widget>) x y)
  (log-format widget
              ":button-release-callback (~A, ~A) is called in <click-button-widget>" x y)
  (if (inside-button-region-p widget (list x y))
      (setf (button-state-of widget)
            nil))
  t)
