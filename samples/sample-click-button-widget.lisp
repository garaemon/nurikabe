;;================================================
;; sample-click-button-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================


(require :nurikabe)

(defvar *manager* (nk:init-gui :loggingp t :threadingp t))
(defvar *win* (nk:make-window :width 300 :height 200
                              :background :white
                              :name "test window"))

(defvar *widget1* (nk:make-widget 'nk:<click-button-widget>
                                 :display-string "click here1"
                                 :parent *win*
                                 :width 200 :height 30
                                 :button-color :skyblue
                                 :x 50 :y 50
                                 :button-callback
                                 #'(lambda ()
                                     (format t "BUTTON PRESSED1!~%"))))

(defvar *widget2* (nk:make-widget 'nk:<click-button-widget>
                                 :display-string "click here2"
                                 :parent *win*
                                 :width 200 :height 30
                                 :button-color :pink
                                 :pressed-color :red
                                 :x 50 :y 100
                                 :button-callback
                                 #'(lambda ()
                                     (format t "BUTTON PRESSED2!~%"))))
                                 

(nk:render-widgets *win*)
