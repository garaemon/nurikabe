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

(defun make-widget (class
                    &rest
                    args
                    &key
                    (parent nil)
                    (x nil) (y nil)
                    (width nil) (height nil)
                    (font "VeraMono.ttf")
                    (map t)
                    &allow-other-keys)
  (if (not (and parent x y width height))
      (error "You have to set :parent, :x, :y, :width and :height in MAKE-WIDGET"))
  (let ((manager (manager-of parent)))
    (let ((widget (apply #'make-instance class
                         :x x :y y
                         :parent parent
                         :width width :height height
                         :font font
                         :manager manager
                         :allow-other-keys t
                         args)))
      (with-x-serialize (manager)
        ;; ここではまだ、<widget>をつくったにすぎない
        ;; 対応するXのオブジェクトを作る
        (setf (xwindow-of widget)
              (x-create-window
               :display (display-of manager)
               :screen (root-screen-of manager)
               :parent (xwindow-of parent)
               :x x :y y
               :width width :height height
               :event-mask (default-event-mask)))
        (setf (gcontext-of widget)
              (clyax::XCreateGC (display-of manager)
                                (xwindow-of widget)
                                0
                                (cffi:null-pointer)))
        (setf (image-array-of widget)
              (foreign-alloc :unsigned-char
                           :count
                           (* (width-of widget) (height-of widget) 4)))
        (setf (image-of widget) (make-image :width width
                                            :height height
                                            :foreground (foreground-of (image-of parent))
                                            :background (background-of (image-of parent))
                                            :font font))
        ;; parentへwidgetを追加
        (add-widget parent widget)
        (if map (map-window widget))
        (flush manager)
        (wait-event manager clyax::Expose)
        ;;(flush manager)
        ;;(render-widget widget)
        (log-format widget "widget ~A is created" widget)
        widget))))

(defmethod debug-mode ((widget <widget>))
  (setf (debugp-of widget) t))

(defmethod toggle-debug-mode ((widget <widget>))
  (if (debugp-of widget)
      (setf (debugp-of widget) nil)
      (setf (debugp-of widget) t)))
