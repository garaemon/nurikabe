;;================================================
;; sample-4.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :nurikabe)

(defvar *manager* (nurikabe:init-gui))
(defvar *win* (nurikabe:make-window :width 300
                                    :height 200
                                    :background :white))
(defvar *gl-canvas* nil)

(defun init ()
  (setf *gl-canvas* (nurikabe::make-gl-canvas *win*
                                              :x 0
                                              :y 0
                                              :width 300
                                              :height 200))
  (glx:make-current                     ;cannot call twice... why?
   (nurikabe::xwindow-of *gl-canvas*)
   (nurikabe::gl-context-of *gl-canvas*))
  
  )

(defmethod nurikabe::resize-callback ((win nurikabe::<window>)
                                      width height)
  (gl:viewport 0.0 0.0
               width height)
  (gl:load-identity)
  (gl:ortho (/ (- width) 200.0)
            (/ width 200.0)
            (/ (- height) 200.0)
            (/ height 200.0)
            -1.0
            1.0)
  )

(defun main ()
  (unless *gl-canvas*
    (init))
  (nurikabe::with-gl-make-current-canvas (*gl-canvas*)
    (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (gl:color-3f 1.0 0.0 0.0)
    (gl:rotate-f 25.0 0.0 1.0 0.0)
    (nurikabe::gl-command
     gl:+polygon+
     (gl:vertex-2f -0.9 -0.9)
     (gl:vertex-2f 0.9 -0.9)
     (gl:vertex-2f 0.9 0.9)
     (gl:vertex-2f -0.9 0.9)
     )
    (glx:swap-buffers)
    )
  (nurikabe:flush *manager*)
  )

(main)
