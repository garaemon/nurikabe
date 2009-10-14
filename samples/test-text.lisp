;;================================================
;; test-text.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
                   (safety 3)))


(require :nurikabe)

(defvar *manager* (nurikabe:init-gui :loggingp t :threadingp nil))
(defvar *win* (nurikabe:make-window :width 300
                                    :height 100
                                    :background :black
                                    :name "test window"))

(nurikabe:draw-string (nurikabe:image-of *win*)
                      "(PICK OBJ *)"
                      0 30
                      :font-size 30
                      :color :white)
(nurikabe:flush-window *win* :clear t)
