;;================================================
;; container-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurikabe)

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

