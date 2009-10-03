;;====================================================================
;; gl-canvas.lisp
;; 
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

(in-package #:nurikabe)

(defmacro with-gl-make-current-canvas ((canvas) &rest args)
  `(progn
     ;;(glx:make-current (xwindow-of ,canvas) (gl-context-of ,canvas))
     ,@args))

(defmacro gl-push-pop-matrix (&rest args)
  `(progn
     (gl:push-matrix)
     (prog1
         (progn ,@args)
       (gl:pop-matrix))))

(defmethod make-gl-canvas ((win <window>)

                           &rest args
                           &key
                           (canvas-class '<gl-canvas>)
                           (x nil)
                           (y nil)
                           (width nil)
                           (height nil)
                           (map t)
                           &allow-other-keys)
  (if (not (and x y width height))
      (error "You have to set :x, :y, :width and :height in MAKE-GL-CANVAS"))
  ;; make canvas object
  (let ((canvas (make-instance canvas-class ;should I call make-widget?
                               :x x :y y
                               :parent win
                               :width width :height height
                               ;; :font font
                               :manager (manager-of win)
                               :allow-other-keys t)))
    ;; make xlib object
    (let ((manager (manager-of win)))
      (let* ((visual (glx:choose-visual (root-screen-of manager)
                                        '(:glx-rgba
                                          (:glx-red-size 1)
                                          (:glx-green-size 1)
                                          (:glx-blue-size 1)
                                          :glx-double-buffer)))
             (colormap (xlib:create-colormap (glx:visual-id visual)
                                             (root-window-of manager))))
        (let ((xwin (xlib:create-window :parent (xwindow-of win)
                                        :x x :y y
                                        :width width :height height
                                        :visual (glx:visual-id visual)
                                        :depth 24
                                        :colormap colormap
                                        :class :input-output
                                        :event-mask (default-event-mask))))
          (xlib:set-wm-properties xwin
                                  :name "glx"
                                  :x x :y y :width width :height height
                                  :min-width width :min-height height
                                  :initial-state :normal)
        (let ((ctx (glx:create-context (root-screen-of manager)
                                       (glx:visual-id visual))))
          (setf (xwindow-of canvas) xwin)
          (setf (gl-context-of canvas) ctx)
          ;; parentへwidgetを追加
          (add-widget win canvas)
          (if map (map-window canvas))
          (log-format canvas "canvas ~A is created" canvas)
          canvas))))))

(defmethod make-gl-canvas ((win <window>)

                           &rest args
                           &key
                           (canvas-class '<gl-canvas>)
                           (x nil)
                           (y nil)
                           (width nil)
                           (height nil)
                           (map t)
                           &allow-other-keys)
  (if (not (and x y width height))
      (error "You have to set :x, :y, :width and :height in MAKE-GL-CANVAS"))
  ;; make canvas object
  (let ((canvas (make-instance canvas-class ;should I call make-widget?
                               :x x :y y
                               :parent win
                               :width width :height height
                               ;; :font font
                               :manager (manager-of win)
                               :allow-other-keys t)))
    ;; make xlib object
    (let ((manager (manager-of win)))
      (let* ((visual (glx:choose-visual (root-screen-of manager)
                                        '(:glx-rgba
                                          (:glx-red-size 1)
                                          (:glx-green-size 1)
                                          (:glx-blue-size 1)
                                          :glx-double-buffer)))
             (colormap (xlib:create-colormap (glx:visual-id visual)
                                             (root-window-of manager))))
        (let ((xwin (xlib:create-window :parent (xwindow-of win)
                                        :x x :y y
                                        :width width :height height
                                        :visual (glx:visual-id visual)
                                        :depth 24
                                        :colormap colormap
                                        :class :input-output
                                        :event-mask (default-event-mask))))
          (xlib:set-wm-properties xwin
                                  :name "glx"
                                  :x x :y y :width width :height height
                                  :min-width width :min-height height
                                  :initial-state :normal)
        (let ((ctx (glx:create-context (root-screen-of manager)
                                       (glx:visual-id visual))))
          (setf (xwindow-of canvas) xwin)
          (setf (gl-context-of canvas) ctx)
          ;; parentへwidgetを追加
          (add-widget win canvas)
          (if map (map-window canvas))
          (log-format canvas "canvas ~A is created" canvas)
          canvas))))))


(defmethod flush-window ((window <gl-canvas>) &key (clear nil))
  nil)
