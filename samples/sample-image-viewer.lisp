;;================================================
;; sample-image-viewer.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :nurikabe)

(defvar *manager* (nk:init-gui :loggingp t :threadingp t))
(defvar *win* (nk:make-window :width 300 :height 200
                              :background :white
                              :name "test window"))

(defvar *image* (nk:make-image-from-file "/home/garaemon/69621.jpg"))

(defvar *widget* (nk:make-widget 'nk::<image-viewer-widget>
                                 :parent *win*
                                 :geometry '(:vertical :upper
                                             :horizontal :upper)
                                 :image *image*
                                   :width 300 :height 200))

(nk:with-x-serialize (*manager*)
  (nk:render-widgets *win*))
