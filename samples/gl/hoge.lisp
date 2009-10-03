;;================================================
;; sample-0.lisp
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

(defun test-animation ()
  (unless *gl-canvas*
    (init))
  (nurikabe::with-gl-make-current-canvas (*gl-canvas*)
    (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (nurikabe::gl-push-pop-matrix
     (nurikabe::gl-command
      gl:+polygon+
      (gl:color-3ub 255 0 0)
      (gl:vertex-2f 0.25s0 0.25s0)
      (gl:color-3ub 0 255 0)
      (gl:vertex-2f 0.75s0 0.25s0)
      (gl:color-3ub 0 0 255)
      (gl:vertex-2f 0.75s0 0.75s0)
      (gl:color-3ub 255 255 255)
      (gl:vertex-2f 0.25s0 0.75s0)
      ))
    (glx:swap-buffers)
    )
  )

(test-animation)
