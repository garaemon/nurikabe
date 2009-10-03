;;========================================
;; window.lisp
;;
;; window.lisp provides <window> class and its methods.
;; 
;;  written by R.Ueda(garaemon@gmail.net)
;;========================================
(declaim (optimize (debug 3) (safety 3)))

(in-package #:nurikabe)

;; for printing
(defmethod print-object ((window <window>) stream)
  (print-unreadable-object (window stream :type t :identity t)
    (format stream "~s ~Ax~A"
            (name-of window)
            (width-of window) (height-of window))))

(defmethod log-format ((window <window>) (str string) &rest args)
  (apply #'log-format (manager-of window) str args))

(defmethod clear-window ((window <window>))
  (xlib:clear-area
   (xwindow-of window)
   :x 0 :y 0
   :width (width-of window)
   :height (height-of window)))

(defmethod map-window ((window <window>))
  "Mapping a window.
   'map' means 'show window'.
  
   This method is a wrapper of xlib:map-window for <window> class."
  (xlib:map-window (xwindow-of window)))

(defmethod map-widgets ((window <window>))
  "Mapping the widgets of window."
  ;; map-subwindows of clx has some bugs, i think.
  ;; so i substitute xlib:map-window for xlib:map-subwindows
  ;;(xlib:map-subwindows (xwindow-of window)))
  (iterate:iter
   (iterate:for w in (widgets-of window))
   (map-window w)
   )
  )
  
(defmethod unmap-window ((window <window>))
  "Unmapping window.
   This method is a wrapper of xlib:unmap-window for <window> class."
  (xlib:unmap-window (xwindow-of window)))

(defun make-window (&key
                    (window-class '<window>)
                    (image nil)         ;image = <image>?
                    (width 300)
                    (height 200)
                    (x 100)
                    (y 100)
                    (manager *manager*)
                    (font "VeraMono.ttf")
                    (foreground :black)
                    (background :white)
                    (name ""))
  "When you want to make a window, you have to call this function.
   For, there are some messy settings in making a window.

   You can use the :image keyword when you already have a
   <image> instance to draw. If you don't use :image keyword,
   <image> instance will be created using :width and :height keyword.

   In some architectures, :x and :y may not work well.
   (Such as Mac OS X.)"
  (let ((ret (make-instance window-class :manager manager :name name)))
    (cond (image
           (setf (height-of ret) (height-of image))
           (setf (width-of ret) (width-of image)))
          ((and width height)
           (setf (height-of ret) height)
           (setf (width-of ret) width))
          (t
           (error "width and heigt, or image is required")))
    (setf (x-of ret) x)
    (setf (y-of ret) y)
    (setf (xwindow-of ret)
          (xlib:create-window :parent (root-window-of manager)
                              :x x :y y
                              :width (width-of ret) :height (height-of ret)
                              :event-mask (default-event-mask)))
    (setf (gcontext-of ret) (xlib:create-gcontext
                             :drawable (root-window-of manager)))
    (setf (image-array-of ret)
          (make-array (* (width-of ret) (height-of ret) 4)
                      :element-type '(UNSIGNED-BYTE 8)
                      :initial-element 0))
    (unless image
      (setf (image-of ret) (make-image :width width
                                       :height height
                                       :foreground foreground
                                       :background background
                                       :font font)))
    (xflush)
    (add-window manager ret)
    (map-window ret)
    (put-image ret (image-of ret) :flush t)
    (log-format ret  "window ~A is created" ret)
    ret))

(defmethod put-image ((window <window>)
                      (image <image>)
                      &key
                      (flush nil))
  "This method only set the image slot of window.
   Do'not bother with xlib:put-image.
   If you want to draw image to the window, you just set :flush t or
   call flush method."
  (setf (image-of window) image)
  (if flush (flush-window window))
  t)

(defmethod get-image ((window <window>) &optional (buffer nil))
  "copy <image> from <window> instance."
  (copy-image (image-of window) buffer))

(defmethod update-image-array ((window <window>))
  "copy content of <image>, that is a #3A simple array,
   to image-array of <window>, that is a #1A simple array."
  (let ((from (content-of (image-of window)))
        (to (image-array-of window)))
    (let ((width (width-of (image-of window)))
          (height (height-of (image-of window))))
      (dotimes (i height)
        (dotimes (j width)
          (setf (aref to (+ (* (+ (* i width) j) 4) 0))
                (aref from i j 2))
          (setf (aref to (+ (* (+ (* i width) j) 4) 1))
                (aref from i j 1))
          (setf (aref to (+ (* (+ (* i width) j) 4) 2))
                (aref from i j 0))
      ))
      window)))

(defmethod flush-window ((window <window>) &key (clear nil))
  "In this flush-window method, image-array is copied from image and
   draw image to window.

   we use xlib:put-image xlib:create-image, xlib:force-display-output here.
   "
  (update-image-array window)           ;content of <image> -> image-array of <window>
  (let ((image (xlib:create-image :data (image-array-of window)
                                  :depth 24
                                  :bits-per-pixel 32
                                  :width (width-of window)
                                  :height (height-of window)
                                  :format :z-pixmap)))
    (xlib:put-image (xwindow-of window)
                    (gcontext-of window)
                    image
                    :x 0
                    :y 0
                    :src-x 0
                    :src-y 0
                    :width (width-of window)
                    :height (height-of window))
    (if clear
        (clear-image (image-of window)))
    t))

(defmethod move ((win <window>) x y)
  "If you want to move window manually,
   you can use this method move.
   I think you have to call flush method in order to actually move the window."
  (setf (xlib:drawable-x (xwindow-of win)) x)
  (setf (xlib:drawable-y (xwindow-of win)) y)
  win)

;; for widget
(defmethod add-widget ((win <window>) (widget <widget>))
  (push widget (widgets-of win))
  (add-window (manager-of win) widget))
  
(defmethod delete-widgets ((win <window>) &key (flush nil))
  (let ((widgets (widgets-of win)))
    (iterate:iter
     (iterate:for w in widgets)
     (unmap-window w))
    (setf (widgets-of win) nil)
    (setf (windows-of (manager-of win))
          (remove-if #'(lambda (x) (member x widgets))
                     (windows-of (manager-of win))))
    (log-format win "delete widgets of ~A" win)
    (when flush
      (flush (manager-of win))
      ;;(nurikabe::xflush))
      )
    t))

(defmethod render-widgets ((win <window>) &key (flush t))
  "rendering widgets of window.
  
  The order of widgets, slot of <window>,
  is reversed, so we have to render the widgets in reversed order."
  (dolist (w (reverse (widgets-of win)))
    (log-format win ":render-widget of ~A is called" w)
    (render-widget w)))

(defmethod find-widget-region ((win <window>) pos)
  "pos --> (x y)"
  (dolist (w (widgets-of win))
    (if (widget-region-p w pos)
        (return-from find-widget-region w))))

;; callbacks
(defmethod exposure-callback ((win <window>) x y width height count)
  t)

(defmethod resize-callback ((win <window>) width height)
  t)

(defmethod button-press-callback ((win <window>) x y)
  t)

(defmethod button-release-callback ((win <window>) x y)
  t)

(defmethod motion-notify-callback ((win <window>) x y code)
  t)

(defmethod configure-notify-callback ((win <window>) x y width height)
  t)
