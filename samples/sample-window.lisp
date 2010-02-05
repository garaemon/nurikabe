;;================================================
;; test-window.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :nurikabe)

(defvar *manager* (nurikabe:init-gui :loggingp t :threadingp t))
(defvar *win* (nurikabe:make-window :width 300
                                    :height 200
                                    :background :white
                                    :name "test window"))
;; move sample
(dotimes (i 300)
  (nk:move *win* i i))
(nk:move *win* 100 100)

;; not works
(dotimes (i 100)
  (nk:set-background-color *win* :black)
  (nk:set-background-color *win* :white)
  )
