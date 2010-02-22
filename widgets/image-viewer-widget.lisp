;;================================================
;; image-viewer-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurikabe)

(defclass* <image-viewer-widget>
    (<container-widget>)
  ((slider-size 15)
   (image-widget nil)                   ;instance of <image-widget>
   (image nil)                          ;target image
   (horizontal-slide-widget nil)        ;instance of <slide-widget>
   (vertical-slide-widget nil)          ;instance of <slide-widget>
   (left-up-position '(0.0 0.0))
   ))


(defmethod init-widget ((widget <image-viewer-widget>))
  (with-slots (image-widget image
               horizontal-slide-widget vertical-slide-widget
               width height slider-size)
      widget
    (let ((image-width (width-of image))
          (image-height (height-of image)))
      (unless image-widget
        (setf image-widget (make-widget '<image-widget>
                                        :lock nil
                                        :wait-expose nil
                                        :parent widget
                                        :x 0 :y 0
                                        :width (- width slider-size)
                                        :height (- height slider-size))))
      (unless horizontal-slide-widget
        (setf horizontal-slide-widget
              (make-widget '<slide-widget>
                           :lock nil :wait-expose nil
                           :whole-width image-width
                           :parent widget
                           :x 0 :y (- height slider-size)
                           :update-callback
                           #'(lambda (x)
                               (setf (left-up-position-of widget)
                                     (list x
                                           (cadr
                                            (left-up-position-of widget))))
                               (log-format widget "-> ~A" x)
                               (render-widget widget))
                           :width (- width slider-size) :height slider-size
                           :verticalp nil)))
      (unless vertical-slide-widget
        (setf vertical-slide-widget
              (make-widget '<slide-widget>
                           :lock nil :wait-expose nil
                           :whole-width image-height
                           :parent widget
                           :x (- width slider-size) :y 0
                           :update-callback
                           #'(lambda (x)
                               (setf (left-up-position-of widget)
                                     (list (car
                                            (left-up-position-of widget))
                                           x))
                               (log-format widget "-> ~A" x)
                               (render-widget widget))
                           :width slider-size :height (- height slider-size)
                           :verticalp t)))
      widget)))

(defmethod update-image-widget ((widget <image-viewer-widget>))
  ;; copy image to image widget
  (with-slots (left-up-position image image-widget slider-size) widget
    (with-slots (width height) image
      (let ((offset-x (* (- width slider-size (width-of widget))
                         (car left-up-position)))
            (offset-y (* (- height slider-size (height-of widget))
                         (cadr left-up-position))))
        (copy-to-image image (image-of image-widget)
                       :offset-x offset-x :offset-y offset-y)))))

(defmethod render-widget ((widget <image-viewer-widget>))
  (update-image-widget widget)
  (render-widgets widget)
  widget)

