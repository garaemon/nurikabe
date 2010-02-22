;;================================================
;; container-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurikabe)
(defclass* <container-widget>
    (<widget>)
  ((widgets nil)
   (geometries nil)))

(defmethod add-geometry ((widget <container-widget>) geo)
  (with-slots (geometries) widget
    (setf geometries (append geometries (list geo)))))

;; the same methods to <window>
;; i think we should make widget-container class for superclass of
;; <window> and <container-widget>
(defmethod add-widget ((c <container-widget>) (widget <widget>))
  (push widget (widgets-of c)))

(defmethod delete-widgets ((win <container-widget>) &key (flush nil))
  (let ((widgets (widgets-of win)))
    (iterate:iter
     (iterate:for w in widgets)
     (unmap-window w))
    (setf (widgets-of win) nil)
    (setf (windows-of (manager-of win))
          (remove-if #'(lambda (x) (member x widgets))
                     (windows-of (manager-of win))))
    (log-format win "delete widgets of ~A" win)
    (when flush
      (flush (manager-of win)))
    t))

(defmethod delete-widget ((win <container-widget>) (target <widget>))
  "delete target widgets"
  (setf (widgets-of win) (remove target (widgets-of win)))
  win)


(defmethod render-widgets ((wid <container-widget>))
  (dolist (w (widgets-of wid))
    (log-format wid ":render-widget of ~A is called" w)
    (render-widget w)))

(defmethod configure-notify-callback ((win <container-widget>) x y w h)
;;   (with-slots (width height geometries) win
;;     (let ((rw (/ w width))
;;           (rh (/ h height)))
;;       (setf width w)
;;       (setf height h)
;;       (dolist (g geometries)
;;         ;; call resize-callback for all widgets in g
;;         (with-slots (widgets) g
;;           (dolist (w widgets)
;;             (resize-callback w rw rh)))
;;         ;; call arrange-widgets
;;         (arrange-widgets g))
;;       t))
  )

