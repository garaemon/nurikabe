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
   #:draw-polygon
   #:draw-rectangle
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
   #:gl-command
   ;;#:tex-image-2d
   #:glteximage2d
   ;; gl-window
   #:make-gl-window
   #:with-gl-make-current-window
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
   #:alpha-blending))

(in-package #:nurikabe)

;; generics
(defgeneric flush (manager))
(defgeneric event-loop (manager))
(defgeneric xlib-window->window (manager win))
