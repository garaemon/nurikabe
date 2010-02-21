;;================================================
;; sample-slide-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :nurikabe)
(defvar *manager* (nk:init-gui :loggingp t :threadingp t))
(defvar *win* (nk:make-window :width 300 :height 200
                              :background :white
                              :name "test window"))

(defvar *widget1* (nk:make-widget 'nk:<slide-widget>
                                  :x 0 :y 185
                                  :whole-width 1000
                                  :width 300 :height 15
                                  :verticalp nil
                                  :parent *win*))

(defvar *widget2* (nk:make-widget 'nk:<slide-widget>
                                  :x 285 :y 0
                                  :whole-width 1000
                                  :width 15 :height 185
                                  :verticalp t
                                  :parent *win*))

(nk::with-x-serialize (*manager*)
  (nk:render-widgets *win*))
