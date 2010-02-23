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

(defclass* <image-viewer-image-widget>
    (<image-widget>)
  ())

(defmethod init-widget ((widget <image-viewer-widget>))
  (with-slots (image-widget image
               horizontal-slide-widget vertical-slide-widget
               width height slider-size)
      widget
    (let ((image-width (width-of image))
          (image-height (height-of image)))
      (unless image-widget
        (let ((geo (make-geometry :vertical :upper
                                  :horizontal :upper
                                  :parent widget)))
          (setf image-widget (make-widget '<image-viewer-image-widget>
                                          :lock nil :wait-expose nil
                                          :parent widget
                                          :geometry geo
                                          :width (- width slider-size)
                                          :height (- height slider-size)))))
      (unless horizontal-slide-widget
        (let ((geo (make-geometry :vertical :lower
                                  :horizontal :upper
                                  :parent widget)))
          (setf horizontal-slide-widget
                (make-widget '<slide-widget>
                             :lock nil :wait-expose nil
                             :whole-width image-width
                             :parent widget
                             :geometry geo
                             :update-callback
                             #'(lambda (x)
                                 (setf (left-up-position-of widget)
                                       (list x
                                             (cadr
                                              (left-up-position-of widget))))
                                 (render-widget widget))
                             :width (- width slider-size) :height slider-size
                             :verticalp nil))))
      (unless vertical-slide-widget
        (let ((geo (make-geometry :vertical :upper
                                  :horizontal :lower
                                  :parent widget)))
          (setf vertical-slide-widget
                (make-widget '<slide-widget>
                             :lock nil :wait-expose nil
                             :whole-width image-height
                             :parent widget
                             :geometry geo
                             :update-callback
                             #'(lambda (x)
                                 (setf (left-up-position-of widget)
                                       (list (car
                                              (left-up-position-of widget))
                                             x))
                                 (render-widget widget))
                             :width slider-size :height (- height slider-size)
                             :verticalp t))))
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

(defmethod resize-widget ((widget <image-viewer-widget>)
                          nw nh ow oh)
  (with-slots (geometries width height) widget
    (let ((w-diff (- nw ow))
          (h-diff (- nh oh)))
      ;; update width and height
      (setf width (+ w-diff width) height (+ h-diff height))
      ;; upate x object
      (resize widget width height)
      ;; update image-widget
      ;;(resize-image-widget widget w-diff h-diff)
      ;; update geometry and sliders
      (dolist (g geometries)
        (arrange-widgets g nw nh ow oh))
      (render-widgets widget)
      t)))

(defmethod resize-widget ((widget <image-viewer-image-widget>)
                          nw nh ow oh)
  (with-slots (width height) widget
    (let ((w-diff (- nw ow))
          (h-diff (- nh oh)))
      ;; update width and height
      (setf width (+ w-diff width) height (+ h-diff height))
      ;; upate x object
      (resize widget width height)
      ;; update image objects
      (rebuild-image widget)
      widget)))

