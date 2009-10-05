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
     ;;(setf (mem-ref attrib :int 10) clyax:GLX_NONE)
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
     (format t "---~%")
     (let* ((visual (clyax:glXChooseVisual
                     (display-of manager)
                     (root-screen-of manager)
                     attrib)))
       (format t "___~%")
       (let (
            (colormap (clyax:XCreateColormap
                       (display-of manager)
                       (root-window-of manager)
                       (foreign-slot-value visual
                                           'clyax::XVisualInfo
                                           'clyax::visual)
                       clyax:AllocNone)))
         (format t "aaa~%")
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
         (format t "piyo~%")
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
          (format t "fuga~%")
          (clyax:XSetNormalHints
           (display-of manager)
           xwin
           sizehints)
          (format t "hoge~%")
          (clyax::XSetStandardProperties
           (display-of manager)
           xwin
           name
           name
           clyax:None
           (cffi:null-pointer)
           0
           sizehints
           ))
         (let ((ctx (clyax:glXCreateContext
                     (display-of manager)
                     visual
                     (cffi:null-pointer)
                     clyax:True)))
           (setf (xwindow-of canvas) xwin)
           (setf (gl-context-of canvas) ctx)
           ;; parentへwidgetを追加
           (add-window manager canvas)
           (if map (map-window canvas))
           (log-format canvas "window ~A is created" canvas)
           canvas)))))))

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
