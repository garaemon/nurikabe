;;================================================
;; nurikabe.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
		   (safety 3)))

(defpackage #:nurikabe
  (:use #:common-lisp #:chimi)
  (:export #:*manager* #:<manager>
           #:init-gui
           #:flush
           #:*color-table* #:symbol->rgb-vector
           #:<window>
           #:make-window
           #:put-image
           #:flush-window
           #:delete-widgets
           #:image-of
           #:<image>
           #:draw-line
           #:draw-string
           #:drawn-string-size
           #:draw-circle
           #:draw-polygon
           #:draw-rectangle
           ;; widget
           #:make-click-button
           #:map-widgets
           #:make-toggle-button
           ;; packing
           #:make-packing-box
           #:line-up
           ))

(in-package #:nurikabe)

;; generics
(defgeneric flush (manager))
(defgeneric event-loop (manager))
(defgeneric xlib-window->window (manager win))
