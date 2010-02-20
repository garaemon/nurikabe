;;================================================
;; nurikabe.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
		   (safety 3)))

(defpackage #:nurikabe
  (:use #:common-lisp #:chimi #:cffi)
  (:nicknames #:nk)
  (:export
   ;; manager.lisp
   #:*manager*
   #:init-gui
   #:flush
   #:new-texture-name
   #:with-x-serialize
   #:add-thread-hook
   ;; color.lisp
   #:find-color
   ;; window.lisp
   #:<window>
   #:widgets-of
   #:make-window
   #:flush-window
   #:delete-widgets
   #:render-widgets
   #:move #:set-background-color
   #:exposure-callback
   #:button-press-callback
   #:button-release-callback
   #:configure-notify-callback
   #:motion-notify-callback
   ;; image.lisp
   #:<image>
   #:make-image
   #:draw-line
   #:draw-circle
   #:draw-string
   #:draw-rectangle
   #:clear-image
   #:make-image-from-file
   ;; widget.lisp
   #:make-widget
   #:render-widget
   ;; image-widget.lisp
   #:<image-widget>
   #:image-of
   #:put-image
   #:get-image
   ;; click-button-widget
   #:<click-button-widget>
   ;; toggle-button-widget
   #:<toggle-button-widget>
   ;; slide widget
   #:<slide-widget>
;;    #:background-of
;;    #:flat-content-of
;;    #:drawn-string-size
;;    #:draw-ring
;;    #:draw-polygon
;;    #:draw-rectangle
;;    #:fill-c-array
;;    #:fill-c-array-reverse


;;    ;; widget
;;    #:<widget>
;;    #:make-widget
;;    #:make-click-button
;;    #:map-widgets
;;    #:make-toggle-button
;;    #:render-widget
;;    ;; packing
;;    #:make-packing-box
;;    #:line-up
;;    ;; gl-window
;;    #:make-gl-window
;;    #:with-gl-make-current-window
;;    #:opengl-setup
;;    #:gl-context-of
;;    ;; gl-light
;;    #:make-gl-light
;;    #:setup-gl-light
;;    #:enable-gl-light
;;    ;; gl-material
;;    #:setup-material
;;    #:find-gl-material
;;    #:gl-material-p
;;    #:transparent
;;    #:alpha-blending
;;    #:symbol->gl-rgb-vector
   ))

(in-package #:nurikabe)

;; generics
(defgeneric flush (manager))
(defgeneric event-loop (manager))
(defgeneric xlib-window->window (manager win))
