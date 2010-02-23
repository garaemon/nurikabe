;;================================================
;; sample-geometry.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :nurikabe)

(defvar *manager* (nk:init-gui :loggingp t :threadingp t))
(defvar *win1* (nk:make-window :width 300 :height 200
                               :background :white
                               :name "test window"))

(defvar *widget1* (nk:make-widget 'nk:<click-button-widget>
                                  :display-string "click here1"
                                  :parent *win1*
                                  :width 200 :height 30
                                  :button-color :skyblue
                                  :x 0 :y 0
                                  :button-callback
                                  #'(lambda ()
                                      (format t "BUTTON PRESSED1!~%"))))

(defvar *widget2* (nk:make-widget 'nk:<click-button-widget>
                                  :display-string "click here2"
                                  :parent *win1*
                                  :width 200 :height 30
                                  :button-color :pink
                                  :pressed-color :red
                                  :x 0 :y 0
                                  :button-callback
                                  #'(lambda ()
                                      (format t "BUTTON PRESSED2!~%"))))

(defvar *geo* (nk:make-geometry :vertical :center
                                :horizontal :fix-center
                                :parent *win1*
                                :widgets (list *widget1* *widget2*)))

(defvar *win2* (nk:make-window :width 300 :height 200
                               :background :white
                               :name "test window"))


(defvar *geo2* (nk:make-geometry :vertical :center
                                 :horizontal :fix-center
                                 :parent *win2*))

(defvar *widget3* (nk:make-widget 'nk:<click-button-widget>
                                  :display-string "click here1"
                                  :parent *win2*
                                  :width 200 :height 30
                                  :button-color :skyblue
                                  :geometry *geo2*
                                  :button-callback
                                  #'(lambda ()
                                      (format t "BUTTON PRESSED1!~%"))))

(defvar *widget4* (nk:make-widget 'nk:<click-button-widget>
                                  :display-string "click here2"
                                  :parent *win2*
                                  :width 200 :height 30
                                  :button-color :pink
                                  :pressed-color :red
                                  :geometry *geo2*
                                  :button-callback
                                  #'(lambda ()
                                      (format t "BUTTON PRESSED2!~%"))))

(nk:with-x-serialize (*manager*)
  (nk:render-widgets *win1*)
  (nk:render-widgets *win2*))
