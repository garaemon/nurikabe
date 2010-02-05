;;================================================
;; sample-toggle-button.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :nurikabe)

(defvar *manager* (nk:init-gui :loggingp t :threadingp t))
(defvar *win* (nk:make-window :width 300 :height 200
                              :background :white
                              :name "test window"))


(defvar *widget1* (nk:make-widget 'nk:<toggle-button-widget>
                                  :display-string "check here"
                                  :parent *win*
                                  :width 200 :height 30
                                  :x 10 :y 10))
(defvar *widget2* (nk:make-widget 'nk:<toggle-button-widget>
                                  :display-string "check here?"
                                  :parent *win*
                                  :width 300 :height 40
                                  :button-font-size 15
                                  :x 0 :y 40))

(nk:render-widgets *win*)
