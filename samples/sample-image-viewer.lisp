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

;; (defvar *image* (nk:make-image :width 600 :height 400
;;                                :background :blue))

;; (nk:draw-line *image* 0 0 1000 1000 :width 8)
(defvar *image* (nk:make-image-from-file "/home/garaemon/69621.jpg"))

(let ((geo (nk:make-geometry :vertical :upper
                             :horizontal :upper
                             :parent *win*)))
  (defvar *widget* (nk:make-widget 'nk::<image-viewer-widget>
                                   :parent *win*
                                   :geometry geo :image *image*
                                   :width 300 :height 200)))

(nk:with-x-serialize (*manager*)
  (nk:render-widgets *win*))
