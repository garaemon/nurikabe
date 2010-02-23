;;====================================================================
;; widget.lisp
;; 
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

(in-package #:nurikabe)

;; <widget> classes
(defclass* <widget>
    (<window-core>)
  ((parent nil)                         ;parent window or widget
   (window nil)                         ;relation to <window>
   (debugp nil))                        ;debugging flag
  )

(defmethod render-widget ((widget <widget>))
  "render widget in window.

   You have to over-write this method in subclasses of <widget>."
  (log-format widget "render-widget is called at ~A" widget))

(defmethod init-widget ((widget <widget>))
  t)

(defmethod add-widget ((widget <widget>) (w <widget>))
  (add-widget (parent-of widget) w))

(defun make-widget (class
                    &rest
                    args                ;args pass to the initializer
                    &key
                    (parent nil)        ;window
                    (x nil) (y nil)     ;position in window
                    (geometry nil)
                    (width nil) (height nil)
                    (depth 24)
                    (map t)
                    (lock t)
                    (wait-expose t)
                    (background :white)
                    &allow-other-keys)
  (if (not (and parent width height))
      (error
       "You have to set :parent, :width and :height in MAKE-WIDGET"))
  (if (null (or (and x y) geometry))
      (error "You have to specify :x and :y or :geometry in MAKE-WIDGET"))
  (let ((manager (manager-of parent)))
    (let ((widget (apply #'make-instance class
                         :x x :y y
                         :width width :height height
                         :parent parent :manager manager
                         :background background
                         :allow-other-keys t
                         args)))
      (with-x-serialize (manager :lock lock)
        ;; make an x object
        (setf (xwindow-of widget)
              (xlib:create-window
               :display (display-of manager)
               :screen (root-screen-of manager)
               :parent (xwindow-of parent)
               :x (or x 0) :y (or y 0)
               :depth depth
               :border-width 0
               :background-pixel (symbol->pixel-value background)
               :width width :height height
               :event-mask (default-event-mask)
               :attribute-mask (default-attribute-mask)))
        (xlib:select-input :display (display-of manager)   ;display
                           :drawable (xwindow-of widget)
                           :mask (default-event-mask))
        (setf (gcontext-of widget)
              (xlib:create-gc :display (display-of manager)
                              :drawable (xwindow-of widget)))
        ;; add widget to parent
        (add-widget parent widget)
        (add-widget manager widget)
        (init-widget widget)
        ;; if geometry is not an object of <geometry>
        (if (not (chimi:derivedp geometry '<geometry>))
            (setq geometry (make-geometry* geometry)))
        ;; if geometry is specified, we need to call add-widget here
        (when geometry
          (add-widget geometry widget)
          (arrange-widgets geometry))
        (if map (map-window widget))
        (render-widget widget)
        (flush manager)
        (if wait-expose (wait-event manager xlib:+expose+))
        (log-format widget "widget ~A is created" widget)
        ;; TODO: finalizer
        widget))))

(defmethod resize-widget ((widget <widget>)
                          new-parent-width new-parent-height
                          old-parent-width old-parent-height)
  "resize-widget is called in arrange-widgets of geometry class."
  (declare (ignore widget new-parent-height new-parent-width
                   old-parent-width old-parent-height))
  t)

(defmethod debug-mode ((widget <widget>))
  (setf (debugp-of widget) t))

(defmethod release-mode ((widget <widget>))
  (setf (debugp-of widget) nil))

(defmethod toggle-debug-mode ((widget <widget>))
  (setf (debugp-of widget) (not (debugp-of widget))))

(defmethod add-widget ((manager <manager>) (widget <widget>))
  "add a window to manager"
  (push widget (widgets-of manager))
  widget)

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
    (when flush (flush (manager-of win)))
    t))

(defmethod delete-widget ((win <window>) (target <widget>))
  "delete target widgets"
  (setf (widgets-of win) (remove target (widgets-of win)))
  win)

(defmethod delete-widget ((win <window>) (name string))
  (setf (widgets-of win) (remove-if #'(lambda (x) (string= name (name-of x)))
                                    (widgets-of win)))
  win)

