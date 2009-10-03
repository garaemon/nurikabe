;;================================================
;; sample-0.lisp
;;
;; only fill window by blue.
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

(defun main ()
  (unless *gl-canvas*
    (init))
  (nurikabe::with-gl-make-current-canvas (*gl-canvas*)
    (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (glx:swap-buffers)
    )
  (nurikabe:flush *manager*)
  )

(main)