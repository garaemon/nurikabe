;;====================================================================
;; gl-wrapper.lisp
;; 
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

;; gl wrapper
(in-package #:gl)

(defmacro draw-block (type &rest args)
  `(progn
     (gl:begin ,type)
     ,@args
     (gl:end)))

(defmacro push-pop-matrix ( &rest args)
  `(progn
     (gl:push-matrix)
     (prog1
         (progn ,@args)
       (gl:pop-matrix))))

(declaim (inline clear-color-fv))
(defun clear-color-fv (v)
  `(gl:clear-color (elt ,v 0) (elt ,v 1) (elt ,v 2) (elt ,v 3)))

