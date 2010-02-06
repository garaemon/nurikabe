;;========================================
;; window.lisp
;;
;; window.lisp provides <window> class and its methods.
;; 
;;  written by R.Ueda(garaemon@gmail.net)
;;========================================
(declaim (optimize (debug 3)
                   (safety 3)))

(in-package #:nurikabe)

;; for printing
(defmethod print-object ((window <window>) stream)
  (print-unreadable-object (window stream :type t :identity t)
    (format stream "~s ~Ax~A"
            (name-of window)
            (width-of window) (height-of window))))

(defmethod log-format ((window <window-core>) (str string) &rest args)
  (apply #'log-format (manager-of window) str args))

(defmethod clear-window ((window <window>))
  (xlib:clear-window
   :display (display-of (manager-of window))
   :drawable (xwindow-of window)
   :x 0 :y 0
   :width (width-of window)
   :height (height-of window)
   :exposurep t))

(defmethod map-window ((window <window-core>))
  "Mapping a window.
'map' means 'show window'.
 This method is a wrapper of map-window in xlib for <window> class."
  (xlib:map-window :display (display-of (manager-of window))
                   :drawable (xwindow-of window)))

(defmethod map-widgets ((window <window>))
  "Mapping the widgets of window."
  ;; map-subwindows of clx has some bugs, i think.
  ;; so i substitute map-window for map-subwindows
  (iterate:iter
   (iterate:for w in (widgets-of window))
   (map-window w)))
  
(defmethod unmap-window ((window <window-core>))
  "Unmapping window.
   This method is a wrapper of unmap-window for <window> class."
  (xlib:unmap-window :display (display-of (manager-of window))
                     :drawable (xwindow-of window)))

(defun make-window (&key
                    (window-class '<window>)
                    (width 300) (height 200) ;size
                    (x 100) (y 100)     ;position
                    (depth 24)
                    (manager *manager*)
                    (foreground :black) (background :white)
                    (name "nurikabe"))          ;title
  "When you want to make a window, you have to call this function.
For, there are some messy settings in making a window.
"
  (let ((ret (make-instance window-class :manager manager :name name)))
    (if (and width height)           ;if width and height specified
        (progn (setf (height-of ret) height)
               (setf (width-of ret) width))
        (error "width and heigt, or image is required"))
    (setf (x-of ret) x (y-of ret) y (background-of ret) background)
    (with-x-serialize (manager)
      (setf (xwindow-of ret)
            (xlib:create-window
             :display (display-of manager)   ;display
             :parent (root-window-of manager) ;parent
             :screen (root-screen-of manager)
             :x x :y y                  ;position
             :depth depth :width width :height height
             :background-pixel (symbol->pixel-value background)
             :event-mask (default-event-mask)
             :attribute-mask (default-attribute-mask)))
      (xlib:select-input :display (display-of manager) ;display
                         :drawable (xwindow-of ret)
                         :mask (default-event-mask))
      (xlib:store-name :display (display-of manager) ;title
                       :drawable (xwindow-of ret)
                       :title name)
      ;; create gc
      (setf (gcontext-of ret)
            (xlib:create-gc :display (display-of manager)
                            :drawable (root-window-of manager)))
      (add-window manager ret)      ;add to manager
      (map-window ret)              ;mapping
      (flush manager)               ;tell mapping of winow to x server
      (log-format ret  "window ~A is created" ret)
      ;; TODO: setup finalizer
      ret)))

(defmethod flush ((window <window>))
  "Flush window."
  (flush (manager-of window)))

(defmethod move ((win <window>) x y)
  "If you want to move window manually,
   you can use this method move."
  (xlib:move-window :display (display-of (manager-of win))
                    :drawable (xwindow-of win)
                    :x x :y y)
  win)

(defmethod set-background-color ((win <window>) color)
  "Currently does not work..."
  (xlib:set-window-background
   :display (display-of (manager-of win))
   :drawable (xwindow-of win)
   :color (find-color color)))

;; for widget
(defmethod add-widget ((win <window>) (widget <widget>))
  (push widget (widgets-of win)))
  
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
      (flush (manager-of win)))
    t))

(defmethod delete-widget ((win <window>) (target <widget>))
  "delete target widgets"
  (setf (widgets-of win) (remove target (widgets-of win)))
  win)

(defmethod delete-widget ((win <window>) (name string))
  (setf (widgets-of win) (remove-if #'(lambda (x) (string= name (name-of x)))
                                    (widgets-of win)))
  win)

(defmethod render-widgets ((win <window>) &key (flush t))
  "rendering widgets of window.
  
  The order of widgets, slot of <window>,
  is reversed, so we have to render the widgets in reversed order."
  (dolist (w (widgets-of win))
    (log-format win ":render-widget of ~A is called" w)
    (render-widget w)))

(defmethod find-widget-region ((win <window>) pos)
  "pos --> (x y)"
  (dolist (w (widgets-of win))
    (if (widget-region-p w pos)
        (return-from find-widget-region w))))

;; callbacks
(defmethod exposure-callback ((win <window-core>) x y width height count)
  t)

(defmethod resize-callback ((win <window-core>) width height)
  t)

(defmethod button-press-callback ((win <window-core>) x y)
  t)

(defmethod button-release-callback ((win <window-core>) x y)
  t)

(defmethod motion-notify-callback ((win <window-core>) x y code)
  t)

(defmethod configure-notify-callback ((win <window-core>) x y width height)
  t)

(defmethod leave-notify-callback ((win <window-core>))
  t)

(defmethod exposure-callback ((win <window>) x y width height count)
  ;; need to re-render ...
  (render-widgets win)
  t)

(defmethod nop-callback ((win <window-core>))
  ;; always called
  t)
