;;================================================
;; nurikabe.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
		   (safety 3)))

(defpackage #:nurikabe
  (:use #:common-lisp #:chimi #:cffi)
  (:export
   ;; manager.lisp
   #:*manager*
   #:init-gui
   #:flush
   #:new-texture-name
   #:with-x-serialize
   ;; classes.lisp
   #:<window>
   #:<image>
   #:<gl-window>
   #:width-of
   #:height-of
   #:x-of
   #:y-of
   #:manager-of
   #:mutex-of
   ;; color.lisp
   #:symbol->rgb-vector
   ;; window.lisp
   #:make-window
   #:put-image
   #:flush-window
   #:delete-widgets
   #:image-of
   #:xwindow-of
   ;; image.lisp
   #:make-image
   #:flat-content-of
   #:draw-line
   #:draw-string
   #:drawn-string-size
   #:draw-circle
   #:draw-ring
   #:draw-polygon
   #:draw-rectangle
   #:fill-c-array
   ;; callbacks.lisp
   #:exposure-callback
   #:button-press-callback
   #:button-release-callback
   #:configure-notify-callback
   #:motion-notify-callback
   ;; widget
   #:make-click-button
   #:map-widgets
   #:make-toggle-button
   ;; packing
   #:make-packing-box
   #:line-up
   ;; gl-wrapper
   #:gl-push-pop-matrix
   #:gl-push-pop-attribute
   #:gl-disable-block
   #:gl-enable-block
   #:gl-clear-color-fv
   #:gl-vertex-3fv
   #:gl-color-3fv
   #:gl-translate-fv
   #:gl-normal-3fv
   #:gl-command
   #:gl-light-fv
   #:gl-material-fv
   #:gl-mult-matrix-f
   #:gl-color-3fv
   #:gl-tex-coord-2fv
   #:gl-new-texture-name
   #:gl-display-list-block
   ;; gl-window
   #:make-gl-window
   #:with-gl-make-current-window
   #:opengl-setup
   #:gl-context-of
   ;; gl-light
   #:make-gl-light
   #:setup-gl-light
   #:enable-gl-light
   ;; gl-material
   #:setup-material
   #:find-gl-material
   #:gl-material-p
   #:transparent
   #:alpha-blending
   #:symbol->gl-rgb-vector
   ))

(in-package #:nurikabe)

;; generics
(defgeneric flush (manager))
(defgeneric event-loop (manager))
(defgeneric xlib-window->window (manager win))
