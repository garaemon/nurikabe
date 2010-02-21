;;================================================
;; gl-window.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package #:nurikabe)

(defun fill-default-glx-attrib (attrib)
  (progn
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
                               :allow-other-keys t))
        (display (display-of manager))
        (screen (root-screen-of manager))
        (root (root-window-of manager)))
    ;; make xlib object
    (with-foreign-object
        (attrib :int 11)
      ;; set attrib
      (fill-default-glx-attrib attrib)
      (let* ((visual (glx:choose-visual
                      :display display
                      :screen screen
                      :attribute attrib))
             (colormap (xlib:create-colormap
                        :display display
                        :window root
                        :visual (xlib:visual-info-visual visual)
                        :alloc xlib:+alloc-none+))
             (attr (xlib:create-set-window-attributes
                    :event-mask (default-event-mask)
                    :background-pixel 0
                    :border-pixel 0
                    :colormap colormap))
             (xwin (xlib:create-window
                    :display display
                    :parent root
                    :x x :y y
                    :width width :height height
                    :border-width 2
                    :depth (xlib:visual-info-depth visual)
                    :class xlib:+input-output+ ;???
                    :attribute-mask (default-attribute-mask)
                    :visual-info visual
                    :attribute attr)))
        ;; set hints and properties
        (let ((size-hints
               (xlib:create-size-hints
                :x x :y y
                :width width :height height
                :flags (logior xlib:+us-size+ xlib:+us-position+)
                :set-normal-hints-p t
                :display display
                :drawable xwin)))
;;;           (xlib:set-standard-properties :display display
;;;                                         :window xwin
;;;                                         :window-name name
;;;                                         :icon-name name
;;;                                         :pixmap xlib:+none+
;;;                                         :size-hints size-hints)
          (let ((ctx (glx:create-context :display display
                                         :visual-info visual
                                         :directp t)))
            
            (setf (xwindow-of canvas) xwin)
            (setf (gl-context-of canvas)
                  (list (cons (chimi:current-thread) ctx)))
            ;; parentへwidgetを追加
            (add-window manager canvas)
            ;;(when map (map-window canvas))
            (map-window canvas)
            (flush manager)
            (wait-event manager xlib:+expose+) ;????
            ;; free C Objects?
            (xlib:free visual)
            (glx:make-current :display display :drawable xwin :glx-context ctx)
            (log-format canvas "window ~A is created" canvas))
          canvas)))))

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
          (let ((visual
                 (glx:choose-visual
                  :display (display-of manager)
                  :screen (root-screen-of manager)
                  :attribute attrib)))
            (push (cons (chimi:current-thread)
                        (glx:create-context
                         :display (display-of manager)
                         :visual-info visual
                         :directp t))
                  gl-context))))
      (let ((current-thread-context (current-thread-gl-context win)))
        (prog1
            (glx:make-current :display (display-of manager)
                              :drawable (xwindow-of win)
                              :glx-context current-thread-context)
          (if need-setup?
              (opengl-setup win)))))))

(defmethod current-thread-gl-context ((win <gl-window>))
  (cdr (assoc (chimi:current-thread) (gl-context-of win))))

(defmacro with-gl-make-current-window ((canvas) &rest args)
  (let ((make-current-result (gensym)))
    `(progn
       (gl-make-current ,canvas)
       (if (not (and (eq (pointer-address (glx:get-current-drawable))
                         (pointer-address (xwindow-of ,canvas)))
                     (eq (pointer-address (glx:get-current-context))
                         (pointer-address (current-thread-gl-context ,canvas)))))
           (let ((,make-current-result (gl-make-current ,canvas))) ;aho
             (if (= ,make-current-result xlib:+true+)
                 nil
                 (progn
                   (format t "make current failed~%")))))
       ,@args)))
