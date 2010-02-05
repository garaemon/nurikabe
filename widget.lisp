;;====================================================================
;; widget.lisp
;; 
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

(in-package #:nurikabe)

(defmethod render-widget ((widget <widget>))
  "render widget in window.

   You have to over-write this method in subclasses of <widget>."
  (log-format widget "render-widget is called at ~A" widget))

(defmethod add-widget ((widget <widget>) (w <widget>))
  (add-widget (parent-of widget) w))

(defun make-widget (class
                    &rest
                    args                ;args pass to the initializer
                    &key
                    (parent nil)        ;window
                    (x nil) (y nil)     ;position in window
                    (width nil) (height nil)
                    (depth 24)
                    (map t)
                    (background :white)
                    &allow-other-keys)
  (if (not (and parent x y width height))
      (error
       "You have to set :parent, :x, :y, :width and :height in MAKE-WIDGET"))
  (let ((manager (manager-of parent)))
    (let ((widget (apply #'make-instance class
                         :x x :y y
                         :width width :height height
                         :parent parent :manager manager
                         :background background
                         :allow-other-keys t
                         args)))
      (with-x-serialize (manager)
        ;; make an x object
        (setf (xwindow-of widget)
              (xlib:create-window
               :display (display-of manager)
               :screen (root-screen-of manager)
               :parent (xwindow-of parent)
               :x x :y y
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
        (if map (map-window widget))
        (flush manager)
        (wait-event manager xlib:+expose+)
        (log-format widget "widget ~A is created" widget)
        ;; TODO: finalizer
        widget))))

(defmethod debug-mode ((widget <widget>))
  (setf (debugp-of widget) t))

(defmethod release-mode ((widget <widget>))
  (setf (debugp-of widget) nil))

(defmethod toggle-debug-mode ((widget <widget>))
  (setf (debugp-of widget) (not (debugp-of widget))))
