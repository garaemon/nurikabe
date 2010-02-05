;;====================================================================
;; font.lisp
;; 
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

(in-package #:nurikabe)

(defun ttf-font-size->pixel-font-width (ttf-font-size font-loader)
  (let ((paths (paths-ttf:paths-from-string
                font-loader "A"
                :scale-x ttf-font-size
                :scale-y (- ttf-font-size)
                :offset (paths-ttf::make-point 0 0))))
    (let ((all-knots
           (reduce #'append
                   (mapcar #'(lambda (x) (coerce (paths::path-knots x) 'cons))
                           paths))))
      (let ((min-x (apply #'min (mapcar #'car all-knots)))
            (max-x (apply #'max (mapcar #'car all-knots))))
        (let ((width-in-pixel (- max-x min-x)))
          width-in-pixel)))))

(defun ttf-font-size->pixel-font-height (ttf-font-size font-loader)
  (let ((paths (paths-ttf:paths-from-string
                font-loader "A"
                :scale-x ttf-font-size
                :scale-y (- ttf-font-size)
                :offset (paths-ttf::make-point 0 0))))
    (let ((all-knots
           (reduce #'append
                   (mapcar #'(lambda (x) (coerce (paths::path-knots x) 'cons))
                           paths))))
      (let ((min-y (apply #'min (mapcar #'cdr all-knots)))
            (max-y (apply #'max (mapcar #'cdr all-knots))))
        (let ((height-in-pixel (- max-y min-y)))
          height-in-pixel)))))

