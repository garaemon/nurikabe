;;================================================
;; sample-5.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :nurikabe)
(require :nurarihyon)
(use-package :nurarihyon)

(defun init ()
  (setf *gl-canvas* (nurikabe:make-gl-window
                     :x 0
                     :y 0
                     :width 300
                     :height 200))

  (glx:make-current                     ;cannot call twice... why?
   (nurikabe:xwindow-of *gl-canvas*)
   (nurikabe:gl-context-of *gl-canvas*))
  (gl:enable gl:+line-smooth+)
  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (reshape 300 200)
  )

(defmethod nurikabe:configure-notify-callback
    ((win nurikabe::<gl-window>)
     x y width height)
  (reshape width height)
  (main)
  )

(defvar *pre-x* 0)
(defvar *pre-y* 0)
(defvar *angle-x* 0.0)
(defvar *angle-y* 0.0)

(defmethod nurikabe:button-press-callback ((win nurikabe::<gl-window>) x y)
  (setf *pre-x* x)
  (setf *pre-y* y)
  t)

(defmethod nurikabe:motion-notify-callback ((win nurikabe::<gl-window>) x y code)
  (let ((theta-y (/ (* 360 (- x *pre-x*)) 200.0))
        (theta-x (/ (* 360 (- *pre-y* y)) 200.0)))
    (setf *angle-x* (- *angle-x* theta-x))
    (setf *angle-y* (+ *angle-y* theta-y))
    (setf *pre-y* y)
    (setf *pre-x* x)
    (main)
    t))

(defun reshape (width height)
  (gl:viewport 0 0 width height)
  (let ((h (coerce (/ height width) 'double-float)))
    (gl:matrix-mode gl:+projection+)
    (gl:load-identity)
    (gl:frustum -1.0d0 1.0d0 (- h) h 5.0d0 60.0d0))
  (gl:matrix-mode gl:+modelview+)
  (gl:load-identity)
  (gl:translate-f 0.2s0 0.0s0 -20.0s0))

(defvar *vertices*
  (list (float-vector 0.0 0.0 0.0)
        (float-vector 1.0 0.0 0.0)
        (float-vector 1.0 1.0 0.0)
        (float-vector 0.0 1.0 0.0)
        (float-vector 0.0 0.0 1.0)
        (float-vector 1.0 0.0 1.0)
        (float-vector 1.0 1.0 1.0)
        (float-vector 0.0 1.0 1.0)))

(defvar *edges*
  '((0 1)
    (1 2)
    (2 3)
    (3 0)
    (4 5)
    (5 6)
    (6 7)
    (7 4)
    (0 4)
    (1 5)
    (2 6)
    (3 7)))

(defvar *hoge* 0.0)

(defun main ()
  (unless *gl-canvas*
    (init))
  (when GLX::*CURRENT-CONTEXT*  
    (nurikabe:with-gl-make-current-window (*gl-canvas*)
    ;;(gl:clear gl:+color-buffer-bit+)
    (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (gl:color-3f 0.0 0.0 0.0)
    (gl::push-pop-matrix
     (gl:rotate-f *angle-x* 1.0 0.0 0.0)
     (gl:rotate-f *angle-y* 0.0 1.0 0.0)
       (nurikabe::gl-command
        gl:+lines+
     (dotimes (i 12)
        (gl::vertex-3fv
         (elt *vertices*
              (elt (elt *edges* i) 0)))
        (gl::vertex-3fv
         (elt *vertices*
              (elt (elt *edges* i) 1)))
        (format t "~A -> ~A~%"
                (elt *vertices*
              (elt (elt *edges* i) 0))
                (elt *vertices*
              (elt (elt *edges* i) 1)))
        )
       ))
    (glx:swap-buffers)
    ))
  (nurikabe:flush *manager*)
  )

;; (defmethod flush-window ((window nurikabe::<gl-window>) &key (clear nil))
;;   (main))

(defvar *manager* (nurikabe:init-gui :loggingp nil))
(defvar *gl-canvas* nil)
(main)
