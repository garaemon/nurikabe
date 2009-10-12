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
  (clyax::XClearArea
   (display-of (manager-of window))
   (xwindow-of window)
   0 0
   (width-of window)
   (height-of window)
   t)
  )

(defmethod map-window ((window <window>))
  "Mapping a window.
   'map' means 'show window'.
  
   This method is a wrapper of map-window for <window> class."
  (clyax::XMapWindow (display-of (manager-of window)) (xwindow-of window)))

(defmethod map-widgets ((window <window>))
  "Mapping the widgets of window."
  ;; map-subwindows of clx has some bugs, i think.
  ;; so i substitute map-window for map-subwindows
  (iterate:iter
   (iterate:for w in (widgets-of window))
   (map-window w)
   )
  )
  
(defmethod unmap-window ((window <window>))
  "Unmapping window.
   This method is a wrapper of unmap-window for <window> class."
  (clyax::XUnmapWindow (display-of (manager-of window)) (xwindow-of window)))

(defun make-window (&key
                    (window-class '<window>)
                    (image nil)         ;image = <image>?
                    (width 300)
                    (height 200)
                    (x 100)
                    (y 100)
                    (depth 24)
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
    (with-x-serialize (manager)
      (setf (x-of ret) x)
      (setf (y-of ret) y)
      (setf (xwindow-of ret)
            (x-create-window
             :display (display-of manager)   ;display
             :parent (root-window-of manager) ;parent
             :screen (root-screen-of manager)
             :x x :y y
             :depth depth
             :width width :height height))
      ;; create gc
      (setf (gcontext-of ret)
            (clyax::XCreateGC (display-of manager)
                              (root-window-of manager)
                              0
                              (cffi:null-pointer)))
      (setf (image-array-of ret)
            (foreign-alloc :unsigned-char
                           :count
                           (* (width-of ret) (height-of ret) 4)))
      ;; XImage
      (setf (ximage-of ret)
            (clyax::XCreateImage              ;memory leak...
             (display-of manager)
             (clyax::XDefaultVisual (display-of manager)
                                    (root-screen-of manager))
             24
             clyax::ZPixmap
             0
             (image-array-of ret)
             width height
             32                                   ;bits-per-pixel
             0))
      (unless image
        (setf (image-of ret) (make-image :width width
                                         :height height
                                         :foreground foreground
                                         :background background
                                         :font font)))
    (add-window manager ret)
    
    (map-window ret)
    (flush manager)
    (wait-event manager clyax::Expose)
    (put-image ret (image-of ret) :flush t)
    (flush manager)
    (log-format ret  "window ~A is created" ret)
    ret)))

(defmethod put-image ((window <window>)
                      (image <image>)
                      &key
                      (flush nil))
  "This method only set the image slot of window.
   Do'not bother with put-image.
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
  (let ((from (image-of window))
        (to (image-array-of window)))
    (fill-c-array from to 4)
    window))

(defmethod flush-window ((window <window>) &key (clear nil))
  "In this flush-window method, image-array is copied from image and
   draw image to window.
   
   we use put-image create-image, force-display-output here."
  (update-image-array window) ;content of <image> -> image-array of <window>
  (let ((image (ximage-of window)))
    (clyax::XPutImage
     (display-of (manager-of window))   ;display
     (xwindow-of window)                ;drawable
     (gcontext-of window)               ;gcontext-of
     image                              ;ximage
     0 0                                ;src x, y
     0 0                                ;dest x. y
     (width-of window) (height-of window)))
  ;; destroy image?
  (if clear (clear-image (image-of window)))
  t)

(defmethod move ((win <window>) x y)
  "If you want to move window manually,
   you can use this method move.
   I think you have to call flush method in order to actually move the window."
  (clyax::XMoveWindow (display-of (manager-of win))
                      (xwindow-of win)
                      x y)
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
