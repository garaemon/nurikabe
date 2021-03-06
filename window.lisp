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

(defclass* <window-core>
    ()
  ((width nil) (height nil)             ;size of window
   (name "")                            ;title of window
   (gcontext nil)                       ;graphics context
   (x nil) (y nil)                      ;position of window
   (manager nil)                        ;reference to manager
   (background nil)                     ;background color
   (xwindow nil))                       ;for x window
  (:documentation
   "superclass of <window> and <widget>.
<window-core> class is an interface to xwindow."))

(defclass* <window>
    (<window-core>)
  ((widgets nil)                        ;the list of <widgets>
   (geometries nil))                    ;the list of <geometry>
  (:documentation
   "<window> class is a container of <widget>.
any contents in <window> is must be realized through <widget>."))

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

(defmethod add-geometry ((window <window>) geo)
  (with-slots (geometries) window
    (setf geometries (append geometries (list geo)))))

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
            (xlib:create-simple-window
             :display (display-of manager)   ;display
             :parent (root-window-of manager) ;parent
             :x x :y y :width width :height height
             :background-pixel (symbol->pixel-value background)
             :foreground-pixel (symbol->pixel-value foreground)))
      (xlib:select-input :display (display-of manager) ;display
                         :drawable (xwindow-of ret)
                         :mask (default-event-mask))
      (xlib:store-name :display (display-of manager) ;title
                       :drawable (xwindow-of ret)
                       :title name)
      ;; create gc
      (setf (gcontext-of ret)
            (xlib:create-gc :display (display-of manager)
                            :drawable (xwindow-of ret)))
      (xlib:set-background :display (display-of manager)
                           :gc (gcontext-of ret)
                           :color (symbol->pixel-value background))
      (xlib:set-foreground :display (display-of manager)
                           :gc (gcontext-of ret)
                           :color (symbol->pixel-value foreground))
      (add-window manager ret)      ;add to manager
      (map-window ret)              ;mapping
      (flush manager)               ;tell mapping of winow to x server
      (log-format ret  "window ~A is created" ret)
      ;; TODO: setup finalizer
      ret)))

(defmethod flush ((window <window>))
  "Flush window.
this method is an aliase to flush method of manager."
  (flush (manager-of window)))

(defmethod move ((win <window-core>) x y)
  "If you want to move window manually,
You can use this method."
  (xlib:move-window :display (display-of (manager-of win))
                    :drawable (xwindow-of win)
                    :x x :y y)
  (update-geometry win)                 ;call here?
  win)

(defmethod resize ((win <window-core>) w h)
  (xlib:resize-window :display (display-of (manager-of win))
                      :drawable (xwindow-of win)
                      :width w :height h)
  (update-geometry win)                 ;call here?
  win)

(defmethod update-geometry ((win <window-core>))
  "update x, y, width and height of <window-core> by asking
X server."
  (with-slots (x y width height manager xwindow) win
    (multiple-value-bind (_x _y _width _height)
        (xlib:get-geometry :display (display-of manager)
                           :drawable xwindow)
      (setf x _x)
      (setf y _y)
      (setf width _width)
      (setf height _height))
    win))

(defmethod set-background-color ((win <window>) color)
  "Currently does not work?"
  (xlib:set-window-background
   :display (display-of (manager-of win))
   :drawable (xwindow-of win)
   :color (find-color color)))

(defmethod render-widgets ((win <window>))
  "rendering widgets of window."
  (dolist (w (widgets-of win))
    (log-format win ":render-widget of ~A is called" w)
    (render-widget w)))

(defmethod get-image ((win <window-core>))
  )

;; callbacks
;; callbacks is defined as methods of <window-core>.
;; callback methods of <window-core> is the pure vertual methods
(defmethod exposure-callback ((win <window-core>) x y width height count)
  (declare (ignore win x y width height count))
  t)

(defmethod button-press-callback ((win <window-core>) x y)
  (declare (ignore win x y))
  t)

(defmethod button-release-callback ((win <window-core>) x y)
  (declare (ignore win x y))
  t)

(defmethod motion-notify-callback ((win <window-core>) x y code)
  (declare (ignore win x y code))
  t)

(defmethod configure-notify-callback ((win <window-core>) x y width height)
  (declare (ignore win x y width height))
  t)

(defmethod leave-notify-callback ((win <window-core>))
  (declare (ignore win))
  t)

(defmethod enter-notify-callback ((win <window-core>))
  (declare (ignore win))
  t)

(defmethod nop-callback ((win <window-core>))
  ;; always called
  (declare (ignore win))
  t)

(defmethod exposure-callback ((win <window>) x y width height count)
  "exposure callback of <window> is defined as alias method to render-widgets."
  (render-widgets win)
  t)

(defmethod configure-notify-callback ((win <window>) x y w h)
  "configure-notify-callback is called when resize event is occurred.
In this method, a window manages geometry relation of widgets.

Window calls resize-callback of widgets in its geometry, 
and the arguments of resize-callback is RATIO of change of width and height."
  ;; width and height changed to w and h
  ;; geometry-ed widgets are rearranged by geometry.
  ;; update width and height
  (with-slots (width height geometries) win
    (let ((old-w width)
          (old-h height))
      ;; update width and height
      (setf width w)
      (setf height h)
      (dolist (g geometries)
        (arrange-widgets g w h old-w old-h)))
    t))

(defmethod add-window ((manager <manager>) (window <window>))
  "add a window to manager"
  (push window (windows-of manager))
  (setf (manager-of window) manager)
  window)
