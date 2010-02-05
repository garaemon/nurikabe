;;================================================
;; sample-image-drawing.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :nurikabe)

(defvar *manager* (nk:init-gui :loggingp t :threadingp t))
(defvar *win* (nk:make-window :width 300 :height 200
                              :background :white
                              :name "test window"))
(defvar *widget1* (nk:make-widget 'nk:<image-widget>
                                 :x 0 :y 0
                                 :width 150 :height 100
                                 :parent *win*
                                 :background :blue))
(defvar *widget2* (nk:make-widget 'nk:<image-widget>
                                 :x 150 :y 0
                                 :width 150 :height 100
                                 :parent *win*
                                 :background :green))
(defvar *widget3* (nk:make-widget 'nk:<image-widget>
                                  :x 0 :y 100
                                  :width 150 :height 100
                                  :parent *win*
                                  :background :red))
(defvar *widget4* (nk:make-widget 'nk:<image-widget>
                                  :x 150 :y 100
                                  :width 150 :height 100
                                  :parent *win*
                                  :background :white))

(nk:draw-line (nk:image-of *widget1*) 0 0 100 100)
(nk:draw-circle (nk:image-of *widget2*) 75 50 30)
(nk:draw-string (nk:image-of *widget3*) "NURIKABE" 0 0 :font-size 30)
(nk:draw-rectangle (nk:image-of *widget4*) 0 0 100 100)

(nk:render-widgets *win*)
