;;====================================================================
;; gl-wrapper.lisp
;; 
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

;; gl wrapper
(require :nurarihyon)
(in-package #:gl)
(export '(push-pop-matrix push-pop-attribute
          enable-block disable-block
          clear-color-fv vertex-3fv translate-fv
          color-3fv 
          normal-3fv
          glu-look-at glu-perspective))
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

(defmacro push-pop-attribute ( &rest args)
  `(progn
     (gl:push-attrib gl:+all-attrib-bits+)
     (prog1
         (progn ,@args)
       (gl:pop-attrib))))

(defmacro disable-block (op &rest args)
  `(progn
     (gl:disable ,op)
     (prog1
         (progn ,@args)
       (gl:enable ,op))
     ))
  

(defmacro enable-block (op &rest args)
  `(progn
     (gl:enable ,op)
     (prog1
         (progn ,@args)
       (gl:disable ,op))
     ))

(defun glu-look-at (eye center up)
  (let* ((f (nurarihyon:normalize-vector (nurarihyon:v- center eye)))
         (up2 (nurarihyon:normalize-vector up)))
    (let* ((s (nurarihyon:normalize-vector (nurarihyon:v* f up2)))
           (u (nurarihyon:normalize-vector (nurarihyon:v* s f))))
      (gl::mult-matrix-f
       (list
        (elt s 0) (elt u 0) (- (elt f 0)) 0.0
        (elt s 1) (elt u 1) (- (elt f 1)) 0.0
        (elt s 2) (elt u 2) (- (elt f 2)) 0.0
        0.0        0.0           0.0      1.0))
      (gl:translate-f (- (elt eye 0))
                      (- (elt eye 1))
                      (- (elt eye 2)))
      )))

(defun glu-perspective (fov aspect znear zfar)
  (let* ((fh (* znear (tan (/ fov 2.0))))
         (fw (* fh aspect)))
    (gl:frustum (coerce (- fw) 'double-float)
                (coerce fw 'double-float)
                (coerce (- fh) 'double-float)
                (coerce fh 'double-float)
                (coerce znear 'double-float)
                (coerce zfar 'double-float))))

(declaim (inline clear-color-fv))
(defun clear-color-fv (v)
  (gl:clear-color (elt v 0) (elt v 1) (elt v 2) (elt v 3)))

(declaim (inline vertex-3fv))
(defun vertex-3fv (v)
  (gl:vertex-3f (elt v 0) (elt v 1) (elt v 2)))

(declaim (inline color-3fv))
(defun color-3fv (v)
  (gl:color-3f (elt v 0) (elt v 1) (elt v 2)))


(declaim (inline translate-fv))
(defun translate-fv (v)
  (gl:translate-f (elt v 0) (elt v 1) (elt v 2)))

(declaim (inline normal-3fv))
(defun normal-3fv (v)
  (gl:normal-3f (elt v 0) (elt v 1) (elt v 2)))

(in-package #:nurikabe)
(defmacro gl-command (type &rest args)
  `(progn
     (gl:begin ,type)
     ,@args
     (gl:end)))
;; we use cffixxxx
;; (sb-alien:load-shared-object
;;  "/System/Library/Frameworks/OpenGL.framework/Versions/A/OpenGL")
;; (sb-alien:load-shared-object
;;  "/usr/X11/lib/libGL.dylib")
;;(sb-alien:load-shared-object
;; "/usr/X11/lib/libGLX.dylib")

;; (use-foreign-library darwin-gl)
;; (use-foreign-library libgl)

;; (defctype void :void)
;; (defctype int :int)
;; (defctype sizei :int)
;; (defctype enum :int)
;; copy from cl-opengl
;;(defcfun ("glTexImage2D" tex-image-2d) :void
;; (sb-alien:define-alien-routine "glTexImage2D" sb-alien:void
;;   (target sb-alien:int)
;;   (level sb-alien:int)
;;   (internalformat sb-alien:int)
;;   (width sb-alien:int)
;;   (height sb-alien:int)
;;   (border sb-alien:int)
;;   (format sb-alien:int)
;;   (type sb-alien:int)
;;   ;;(pixels (:pointer void))
;;   (pixels (sb-alien:* sb-alien:unsigned-char))
;;   ;;(pixels (sb-alien:* sb-alien:void))
;;   ;;(pixels sb-alien:void)
;;   )

;; (sb-alien:define-alien-routine
;;  "glTexParameteri" sb-alien:void
;;  (a sb-alien:unsigned-int)
;;  (b sb-alien:unsigned-int)
;;  (c sb-alien:int)
;;  )
