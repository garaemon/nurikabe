;;================================================
;; image-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurikabe)

(defclass* <image-widget>
    (<widget>)
  ((image nil)                          ;an instance of <image>
   (ximage nil)                         ;a c object
   (image-array nil)                    ;c content of ximage
   ))

(defmethod initialize-instance :after ((widget <image-widget>) &rest initargs)
  (declare (ignore initargs))
  (unless (image-array-of widget)
    (setf (image-array-of widget)
          (cffi:foreign-alloc :unsigned-char
                              :count (* (width-of widget)
                                        (height-of widget)
                                        4))))
  ;; make c XImage object
  (unless (ximage-of widget)
    (let ((manager (manager-of widget)))
      (setf (ximage-of widget)
            (xlib:create-image :display (display-of manager)
                               :visual (xlib:default-visual
                                           :display (display-of manager)
                                         :screen (root-screen-of manager))
                               :depth 24
                               :format xlib:+z-pixmap+
                               :offset 0
                               :data (image-array-of widget)
                               :width (width-of widget)
                               :height (height-of widget)
                               :bitmap-pad 32
                               :bytes-per-line 0))))
  ;; make empty image
  (unless (image-of widget)
    (setf (image-of widget)
          (make-image :width (width-of widget)
                      :height (height-of widget)
                      :background (background-of widget))))
  widget)

(defmethod initialize-image ((widget <image-widget>))
  (when (image-array-of widget)
    )
  (when (ximage-of widget)
    )
  )

(defmethod put-image ((widget <image-widget>) (image <image>)
                      &key (flush nil) (lock t))
  (setf (image-of widget) image)
  (render-widget widget)
  (if flush (flush widget))
  t)

(defmethod get-image ((widget <image-widget>))
  (image-of widget))

(defmethod update-image ((widget <image-widget>))
  "copy from <image> instance to x object in <image-widget> slot."
  (let ((from (image-of widget))        ;<image> instance
        (to (image-array-of widget)))   ;c object
    (fill-c-array from to 4))
  widget)

(defmethod render-widget ((widget <image-widget>))
  (update-image widget)                 ;copy to c world...
  ;; copy to x world...
  (xlib:put-image :display (display-of (manager-of widget))
                  :drawable (xwindow-of widget)
                  :gcontext (gcontext-of widget)
                  :image (ximage-of widget)
                  :src-x 0 :src-y 0 :dest-x 0 :dest-y 0
                  :width (width-of widget) :height (height-of widget))
  )
