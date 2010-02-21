;;================================================
;; gl-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurikabe)

(defclass* <gl-widget>
    (<widget>)
  ((gl-contexts nil)                    ;((thread-id . gl-context) ...)
   (glbackground nil)                   ;memorize background color vector
   ;; flags for gl settings
   (lights nil)
   (shading-mode :smooth)
   (depth-mode :less)
   (antialiase-mode :line-smooth)
   (cull-face :back)
   (front-face :ccw)
   ))

(defun fill-default-glx-attrib (attrib)
  (setf (mem-aref attrib :int 0) glx:+rgba+)
  (setf (mem-aref attrib :int 1) glx:+red-size+)
  (setf (mem-aref attrib :int 2) 8)
  (setf (mem-aref attrib :int 3) glx:+green-size+)
  (setf (mem-aref attrib :int 4) 8)
  (setf (mem-aref attrib :int 5) glx:+blue-size+)
  (setf (mem-aref attrib :int 6) 8)
  (setf (mem-aref attrib :int 7) glx:+doublebuffer+)
  (setf (mem-aref attrib :int 8) glx:+depth-size+)
  (setf (mem-aref attrib :int 9) 8)
  (setf (mem-aref attrib :int 10) xlib:+none+)
  attrib)

(defmethod current-thread-gl-context ((wid <gl-widget>))
  (cdr (assoc (chimi:current-thread) (gl-contexts-of wid))))

(defmethod need-make-gl-context-p ((wid <gl-widget>))
  (null (current-thread-gl-context wid)))

(defmethod add-gl-context ((wid <gl-widget>) context)
  (with-slots (gl-contexts) wid
    (push (cons (chimi:current-thread) context) gl-contexts)))

(defmethod gl-make-current ((wid <gl-widget>))
  (with-slots (manager) wid
    (let ((need-setup? (need-make-gl-context-p wid)))
      (when need-setup?
        ;; if does not have gl context fot this thread,
        ;; we need to make another gl context fot it.
        (add-gl-context wid (make-gl-context manager)))
      (let ((current-context (current-thread-gl-context wid)))
        (prog1
            (glx:make-current :display (display-of manager)
                              :drawable (xwindow-of wid)
                              :glx-context current-context)
          (if need-setup?
              (opengl-setup wid)))))))

(defmethod culling-mode-setup ((widget <gl-widget>))
  (with-slots (cull-face) widget
    (when cull-face
      (gl:enable gl:+cull-face+)
      (case cull-face
        (:back (gl:cull-face gl:+back+))
        (:front (gl:cull-face gl:+front+))))))

(defmethod front-face-setup ((widget <gl-widget>))
  (with-slots (front-face) widget
    (case front-face
      (:ccw (gl:front-face gl:+ccw+))
      (:cw (gl:front-face gl:+cw+)))))

(defmethod depth-mode-setup ((widget <gl-widget>))
  (with-slots (depth-mode) widget
    (when depth-mode
      (gl:enable gl:+depth-test+)
      (case depth-mode
        (:never (gl:depth-func gl:+never+))
        (:always (gl:depth-func gl:+always+))
        (:equal (gl:depth-func gl:+equal+))
        (:gequal (gl:depth-func gl:+gequal+))
        (:greater (gl:depth-func gl:+greater+))
        (:not-equal (gl:depth-func gl:+not-equal+))
        (:less (gl:depth-func gl:+less+))
        (t (error "unkown depth mode"))))))

(defmethod shading-mode-setup ((widget <gl-widget>))
  (with-slots (shading-mode) widget
    (case shading-mode
      (:flat (gl:shade-model gl:+flat+))
      (:smooth (gl:shade-model gl:+smooth+)))))

(defmethod antialiase-mode-setup ((widget <gl-widget>))
  (with-slots (antialiase-mode) widget
    (case antialiase-mode
      (:line-smooth
       (gl:enable gl:+line-smooth+)
       (gl:enable gl:+blend+)           ;here?
       (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+))
      (:multi-sample
       (gl:enable gl:+multisample+)))))

(defmethod texture-map-setting ((widget <gl-widget>))
  (gl:tex-parameter-i gl:+texture-2d+ gl:+texture-wrap-s+ gl:+repeat+)
  (gl:tex-parameter-i gl:+texture-2d+ gl:+texture-wrap-t+ gl:+repeat+)
  (gl:tex-parameter-i gl:+texture-2d+ gl:+texture-mag-filter+ gl:+nearest+)
  (gl:tex-parameter-i gl:+texture-2d+ gl:+texture-min-filter+ gl:+nearest+)
  (gl:tex-env-i gl:+texture-2d+ gl:+texture-env-mode+ gl:+decal+)
  (gl:pixel-store-i gl:+unpack-alignment+ 1))

(defmethod clear-color-setup ((widget <gl-widget>))
  (with-slots (background glbackground) widget
    (when background
      (unless glbackground
        (setf glbackground (symbol->gl-rgb-vector background)))
      (gl:clear-color-dv glbackground))))

(defmethod lighting-setup ((widget <gl-widget>))
  (with-slots (lights) widget
    (dolist (l lights)
      (setup-gl-light l))
    lights))

(defmethod glclear ((widget <gl-widget>))
  (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+)))

(defmethod glswap ((widget <gl-widget>))
  (glx:swap-buffers :display (display-of (manager-of widget))
                    :drawable (xwindow-of widget)))

(defmethod opengl-setup ((widget <gl-widget>))
  (clear-color-setup widget)
  (gl:enable gl:+normalize+)
  (gl:enable gl:+auto-normal+)
  (front-face-setup widget)
  (culling-mode-setup widget)
  (depth-mode-setup widget)
  (shading-mode-setup widget)
  (antialiase-mode-setup widget)
  (texture-map-setting widget)
  (lighting-setup widget)
  (glclear widget)
  (glswap widget)
  (glclear widget)
  (if (lights-of widget) (gl:enable gl:+lighting+))
  (glswap widget)
  t)

;; special widget make function for gl-widget and subclass of it.
(defun make-gl-widget (class
                       &rest args
                       &key
                       (parent nil)
                       (x nil) (y nil)
                       (width nil) (height nil)
                       (lock t) (wait-expose t)
                       (map t) (background :black)
                       &allow-other-keys)
  (if (not (and x y width height parent))
      (error "You have to specify :parent, :x, :y, :width and :height ~
in MAKE-GL-WIDGET"))
  (with-slots (manager) parent
    (let ((widget (apply #'make-instance class
                         :x x :y y
                         :width width :height height
                         :manager manager
                         :background background
                         :allow-other-keys t
                         args)))
      (with-x-serialize (manager :lock lock)
        (with-slots (display) manager
          (let* ((visual (make-glx-visual manager))
                 (attr (make-set-window-attributes manager visual)))
            (let ((xwin (xlib:create-window
                         :display display
                         :parent (xwindow-of parent)
                         :x x :y y
                         :width width :height height
                         :border-width 2
                         :depth (xlib:visual-info-depth visual)
                         :background-pixel (symbol->pixel-value background)
                         :class xlib:+input-output+
                         :attribute-mask (default-attribute-mask)
                         :visual-info visual
                         :attribute attr)))
              (setf (xwindow-of widget) xwin)
              (add-widget parent widget)
              (add-widget manager widget)
              (init-widget widget)
              (if map (map-window widget))
              (render-widget widget)
              (flush manager)
              (if wait-expose (wait-event manager xlib:+expose+))
              (xlib:free visual)
              (log-format widget "widget ~A is created" widget)
              ;; TODO: finalizer
              ))))
      widget)))

(defmacro with-gl-make-current ((widget) &rest args)
  (let ((make-current-result (gensym)))
    `(progn
       (gl-make-current ,widget)
       (if (not (and (eq (pointer-address (glx:get-current-drawable))
                         (pointer-address (xwindow-of ,widget)))
                     (eq (pointer-address (glx:get-current-context))
                         (pointer-address (current-thread-gl-context
                                           ,widget)))))
           (let ((,make-current-result (gl-make-current ,widget))) ;aho
             (if (= ,make-current-result xlib:+true+)
                 nil
                 (progn
                   (format t "make current failed~%")))))
       ,@args)))

(defmethod glflush ((widget <gl-widget>))
  (gl:flush)
  (glx:swap-buffers
   :display (nurikabe::display-of (manager-of widget))
   :drawable (xwindow-of widget)))

(defmethod glrender ((widget <gl-widget>))
  t)

(defmethod render-widget ((widget <gl-widget>))
  (log-format widget "render widget called ~A" widget)
  (with-gl-make-current (widget)
    ;; first of all, need to clear
    (glclear widget)
    ;; .. call draw commands...
    (glrender widget)
    (glflush widget)))

;; gl wrapper
(defmethod make-glx-visual ((manager <manager>))
  (with-foreign-object
      (attrib :int 11)
    (fill-default-glx-attrib attrib)
    ;; using old api
    (glx:choose-visual :display (display-of manager)
                       :screen (root-screen-of manager)
                       :attribute attrib)))

(defmethod make-gl-context ((manager <manager>)
                            &key (visual (make-glx-visual manager)))
  ;; using old api
  (glx:create-context :display (display-of manager)
                      :visual-info visual :directp t))

(defmethod make-set-window-attributes ((manager <manager>) visual)
  (with-slots (display root-window) manager
    (let* ((colormap (xlib:create-colormap
                      :display display
                      :window root-window
                      :visual (xlib:visual-info-visual visual)
                      :alloc xlib:+alloc-none+))
           (attr (xlib:create-set-window-attributes
                    :event-mask (default-event-mask)
                    :background-pixel 0
                    :border-pixel 0
                    :colormap colormap)))
      attr)))

