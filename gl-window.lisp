;;================================================
;; gl-window.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package #:nurikabe)

(defun fill-default-glx-attrib (attrib)
  (progn
     (setf (mem-aref attrib :int 0) clyax:GLX_RGBA)
     (setf (mem-aref attrib :int 1) clyax:GLX_RED_SIZE)
     (setf (mem-aref attrib :int 2) 1)
     (setf (mem-aref attrib :int 3) clyax:GLX_GREEN_SIZE)
     (setf (mem-aref attrib :int 4) 1)
     (setf (mem-aref attrib :int 5) clyax:GLX_BLUE_SIZE)
     (setf (mem-aref attrib :int 6) 1)
     (setf (mem-aref attrib :int 7) clyax:GLX_DOUBLEBUFFER)
     (setf (mem-aref attrib :int 8) clyax:GLX_DEPTH_SIZE)
     (setf (mem-aref attrib :int 9) 1)
     (setf (mem-aref attrib :int 10) clyax:None)
     attrib))

(defun make-gl-window (&rest args
                       &key
                       (window-class '<gl-window>)
                       (x nil)
                       (y nil)
                       (name "glx")
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
    (with-foreign-object
        (attrib :int 11)
      ;; set attrib
      (fill-default-glx-attrib attrib)
      (let* ((visual (clyax:glXChooseVisual
                      (display-of manager)
                      (root-screen-of manager)
                      attrib)))
        (let ((colormap (clyax:XCreateColormap
                         (display-of manager)
                         (root-window-of manager)
                         (foreign-slot-value visual
                                             'clyax::XVisualInfo
                                             'clyax::visual)
                         clyax:AllocNone)))
          (with-foreign-object
              (attr 'clyax::XSetWindowAttributes)
            (setf (foreign-slot-value attr
                                      'clyax::XSetWindowAttributes
                                      'clyax::event_mask)
                  (default-event-mask))
           (setf (foreign-slot-value attr
                                     'clyax::XSetWindowAttributes
                                     'clyax::background_pixel)
                 0)
           (setf (foreign-slot-value attr
                                     'clyax::XSetWindowAttributes
                                     'clyax::border_pixel)
                 0)
           (setf (foreign-slot-value attr
                                     'clyax::XSetWindowAttributes
                                     'clyax::colormap)
                 colormap)
           (let ((xwin (clyax:XCreateWindow
                        (display-of manager)         ;display
                        (root-window-of manager)     ;parent
                        x y                          ;x, y
                        width height                 ;width, height
                        2                            ;border width
                        (foreign-slot-value visual
                                            'clyax::XVisualInfo
                                            'clyax::depth)
                        clyax:InputOutput
                        (foreign-slot-value visual
                                            'clyax::XVisualInfo
                                            'clyax::visual)
                        (default-attribute-mask)
                        attr)))
         ;; set hints and properties
         (with-foreign-object
          (sizehints 'clyax::XSizeHints)
          (setf (foreign-slot-value sizehints
                                    'clyax::XSizeHints
                                    'clyax::x)
                x)
          (setf (foreign-slot-value sizehints
                                    'clyax::XSizeHints
                                    'clyax::y)
                y)
          (setf (foreign-slot-value sizehints
                                    'clyax::XSizeHints
                                    'clyax::width)
                width)
          (setf (foreign-slot-value sizehints
                                    'clyax::XSizeHints
                                    'clyax::height)
                height)
          (setf (foreign-slot-value sizehints
                                    'clyax::XSizeHints
                                    'clyax::flags)
                (logior clyax:USSize clyax:USPosition))
          (clyax:XSetNormalHints
           (display-of manager)
           xwin
           sizehints)
          (clyax::XSetStandardProperties
           (display-of manager)
           xwin
           name
           name
           clyax:None
           (cffi:null-pointer)
           0
           sizehints))
         (let ((ctx (clyax:glXCreateContext (display-of manager)
                                            visual
                                            (cffi:null-pointer)
                                            clyax:TRUE)))
           (clyax:XFree visual)
           (setf (xwindow-of canvas) xwin)
           (setf (gl-context-of canvas) (list (cons (chimi:current-thread) ctx)))
           ;; parentへwidgetを追加
           (add-window manager canvas)
           (when map (map-window canvas))
           (flush manager)
           (sleep 1)                    ;TODO. wait map.
           (clyax:glxMakeCurrent (display-of manager) xwin ctx)
           (log-format canvas "window ~A is created" canvas)
           canvas))))))))

(defmethod flush-window ((window <gl-window>) &key (clear nil))
  nil)

(defmethod opengl-setup ((win <gl-window>))
  "You have to override this method in the subclasses of <gl-window>.
  This method will be called ..."
  t)

(defmethod gl-make-current ((win <gl-window>))
  (with-slots (gl-context manager)
      win
    ;; check MT
    (let ((need-setup? (null (current-thread-gl-context win))))
      (when need-setup?
      ;; initialize in sub thread
      ;; make gl context
        (with-foreign-object
            (attrib :int 11)
          ;; set attrib
          (fill-default-glx-attrib attrib)
          (let ((visual (clyax:glXChooseVisual
                         (display-of manager)
                         (root-screen-of manager)
                         attrib)))
            (push (cons (chimi:current-thread)
                        (clyax:glXCreateContext
                         (display-of manager)
                         visual
                         (cffi:null-pointer)
                         clyax:TRUE))      ;TODO pixmap
                  gl-context))))
      (let ((current-thread-context (current-thread-gl-context win)))
        (prog1
            (clyax:glXMakeCurrent (display-of manager) (xwindow-of win) current-thread-context)
          (if need-setup?
              (opengl-setup win)))))))

(defmethod current-thread-gl-context ((win <gl-window>))
  (cdr (assoc (chimi:current-thread) (gl-context-of win))))

(defmacro with-gl-make-current-window ((canvas) &rest args)
  (let ((make-current-result (gensym)))
    `(progn
       (gl-make-current ,canvas)
       (if (not (and (eq (pointer-address (clyax:glxGetCurrentDrawable))
                         (pointer-address (xwindow-of ,canvas)))
                     (eq (pointer-address (clyax:glxGetCurrentContext))
                         (pointer-address (current-thread-gl-context ,canvas)))))
           (let ((,make-current-result (gl-make-current ,canvas))) ;aho
             (if (= ,make-current-result clyax::True)
                 nil
                 (progn
                   (format t "make current failed~%")))))
       ,@args)))

