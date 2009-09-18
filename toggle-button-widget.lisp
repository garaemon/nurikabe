(in-package :nurikabe)

(defmethod draw-check-box ((widget <toggle-button-widget>))
  ;;<--------> display-string-offset
  ;; +----+
  ;; |    |   hoge hoge
  ;; +----+
  ;; 
  (let ((size (/ (display-string-offset-of widget) 2))
        (offset (/ (display-string-offset-of widget) 4)))
    (draw-rectangle (image-of widget)
                    offset offset
                    size size
                    :color :black
                    :fill nil)
    (when (button-state-of widget)
      (draw-line (image-of widget)
                 offset offset
                 (+ size offset) (+ size offset)
                 :color :black)
      (draw-line (image-of widget)
                 offset (+ size offset)
                 (+ size offset) offset
                 :color :black))))

(defmethod render-widget ((widget <toggle-button-widget>))
  (let ((string-bbox (drawn-string-bounding-box
                      (font-loader-of (image-of (parent-of widget)))
                      (display-string-of widget)
                      0 0
                      :font-size (font-size-of widget))))
    ;; string-bbox -> (0, 0)に描画するときのbounding box.
    ;; これを左上を(in-x, in-y)にそろえこと事を考える
    (if (debugp-of widget)
        (draw-rectangle (image-of widget)
                        0
                        0
                        (width-of widget)
                        (height-of widget)
                        :color :red :fill nil))
    ;; draw check box
    (draw-check-box widget)
    (draw-string (image-of widget)
                 (display-string-of widget)
                 (+ (- (cdr (assoc :min-x string-bbox)))
                    (display-string-offset-of widget)) ;x
                 (- (cdr (assoc :min-y string-bbox)))
                 :font-size (font-size-of widget))))

(defmethod make-toggle-button ((win <window>)
                               display-string
                               &key
                               (x nil)
                               (y nil)
                               (font-size 0.01))
  ;; calc width, height
  (let ((string-bbox (drawn-string-bounding-box (font-loader-of (image-of win))
                                                display-string
                                                0 0
                                                :font-size font-size))
        (display-string-offset 20))
    (let ((width (+ (abs (- (cdr (assoc :max-x string-bbox))
                            (cdr (assoc :min-x string-bbox))))
                    display-string-offset))
          (height (abs (- (cdr (assoc :max-y string-bbox))
                          (cdr (assoc :min-y string-bbox))))))
      (make-widget '<toggle-button-widget>
                   :width (round width)
                   :height (round height)
                   :x (round x)
                   :y (round y)
                   :parent win
                   :display-string-offset display-string-offset
                   :display-string display-string
                   :font-size font-size
                   :button-region
                   (let ((size (/ display-string-offset 2))
                         (offset (/ display-string-offset 4)))
                     (list (list offset offset)
                           (list (+ size offset) (+ size offset))))))))

(defmethod toggle-button-state ((widget <toggle-button-widget>))
  (if (button-state-of widget)
      (setf (button-state-of widget) nil)
      (setf (button-state-of widget) t)))

(defmethod inside-button-region-p ((widget <toggle-button-widget>) pos)
  (let ((x (car pos))
        (y (cadr pos)))
    (let ((min-x (car (car (button-region-of widget))))
          (min-y (cadr (car (button-region-of widget))))
          (max-x (car (cadr (button-region-of widget))))
          (max-y (cadr (cadr (button-region-of widget)))))
      (and (< min-x x)
           (< min-y y)
           (> max-x x)
           (> max-y y)))))

(defmethod button-press-callback ((widget <toggle-button-widget>) x y)
  (log-format widget
              :info
              ":button-press-callback (~A, ~A) is called in <toggle-button-widget>" x y)
  ;; x, yはローカル座標になってる
  (if (inside-button-region-p widget (list x y))
      (progn
        (log-format widget :info "inside button region")
        (toggle-button-state widget)
        ))
  t)
