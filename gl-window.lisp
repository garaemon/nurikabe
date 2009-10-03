;;================================================
;; gl-window.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package #:nurikabe)

(defun make-gl-window (&rest args
                       &key
                       (window-class '<gl-window>)
                       (x nil)
                       (y nil)
                       (width nil)
                       (height nil)
                       (manager *manager*)
                       (map t)
                       &allow-other-keys)
  (if (not (and x y width height))
      (error "You have to set :x, :y, :width and :height in MAKE-GL-CANVAS"))
  ;; make canvas object
  (let ((canvas (make-instance window-class
                               :x x :y y
                               :width width :height height
                               ;; :font font
                               :manager manager
                               :allow-other-keys t)))
    ;; make xlib object
    (let* ((visual (glx:choose-visual (root-screen-of manager)
                                      '(:glx-rgba
                                        (:glx-red-size 1)
                                        (:glx-green-size 1)
                                        (:glx-blue-size 1)
                                        ;;(:glx-depth-size 16)
                                        :glx-double-buffer)))
             (colormap (xlib:create-colormap (glx:visual-id visual)
                                             (root-window-of manager))))
        (let ((xwin (xlib:create-window :parent (root-window-of manager)
                                        :x x :y y
                                        :class :input-output
                                        :width width :height height
                                        :visual (glx:visual-id visual)
                                        :depth 24
                                        :colormap colormap
                                        :event-mask
                                        (xlib:make-event-mask :exposure
                                                              :button-press
                                                              :button-release
                                                              :structure-notify
                                                              :button-1-motion
                                                              :substructure-notify))))

                                        ;;:event-mask '(:structure-notify :exposure))))
          (xlib:set-wm-properties xwin
                                  :name "glx"
                                  :resource-class "glx"
                                  :command (list "glx")
                                  :x x :y y :width width :height height
                                  :min-width 100 :min-height 100
                                  :initial-state :normal)
        (let ((ctx (glx:create-context (root-screen-of manager)
                                       (glx:visual-id visual))))
          (setf (xwindow-of canvas) xwin)
          (setf (gl-context-of canvas) ctx)
          ;; parentへwidgetを追加
          (add-window manager canvas)
          (if map (map-window canvas))
          (log-format canvas "window ~A is created" canvas)
          canvas)))))

(defmethod flush-window ((window <gl-window>) &key (clear nil))
  nil)

(defmacro with-gl-make-current-window ((canvas) &rest args)
  `(progn
     (when (not (eq (current-gl-context-of *manager*)
                  ,canvas))
       (glx:make-current (xwindow-of ,canvas)
                         (gl-context-of ,canvas))
       (setf (current-gl-context-of *manager*) ,canvas))
     ,@args))
