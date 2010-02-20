;;================================================
;; button-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurikabe)

;; button widget
(defclass* <button-widget>
    (<image-widget>)
  ((button-state nil)
   (button-region nil)) ;(left-up-x left-up-y right-down-x right-down-y)
  )

(defmethod button-left-up-x ((widget <button-widget>))
  (car (button-region-of widget)))
(defmethod button-left-up-y ((widget <button-widget>))
  (cadr (button-region-of widget)))
(defmethod button-right-down-x ((widget <button-widget>))
  (caddr (button-region-of widget)))
(defmethod button-right-down-y ((widget <button-widget>))
  (cadddr (button-region-of widget)))

(defmethod inside-button-region-p ((widget <button-widget>) x y)
  (and (< (button-left-up-x widget) x (button-right-down-x widget))
       (< (button-left-up-y widget) y (button-right-down-y widget))))


  