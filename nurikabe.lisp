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
   ))

