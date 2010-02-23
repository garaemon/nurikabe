;;================================================
;; sample-gl-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :nurikabe)


;;
;; + <--  +
;;        |
;; + -->  +
;;
(defmethod nk:glrender ((widget nk:<gl-widget>))
  (gl:command
   gl:+polygon+
   (gl:vertex-3dv (nh:double-vector -0.9 -0.9 0.0))
   (gl:vertex-3dv (nh:double-vector 0.9 -0.9 0.0))
   (gl:vertex-3dv (nh:double-vector 0.9 0.9 0.0))
   (gl:vertex-3dv (nh:double-vector -0.9 0.9 0.0))
   )
  )

(defvar *manager* (nk:init-gui :loggingp t :threadingp t))
(defvar *win* (nk:make-window :width 300 :height 200
                              :background :white
                              :name "test window"))

(defvar *widget* (nk:make-gl-widget 'nk:<gl-widget>
                                    :width 300 :height 200
                                    :x 0 :y 0
                                    :background :green
                                    :parent *win*))

(nk:with-x-serialize (*manager*)
  (nk:render-widgets *win*))
