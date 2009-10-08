;;================================================
;; gl-light.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package #:nurikabe)

(defclass* <gl-light>
    ()
  ((ambient nil)
   (diffuse nil)
   (specular nil)
   (position nil)
   (id 0)))

(defun make-gl-light (id
                      &key
                      (ambient '(0.5 0.5 0.5 1.0))
                      (diffuse '(0.8 0.8 0.8 0.8))
                      (specular '(0.7 0.7 0.7 1.0))
                      (position '(1000.0 2000.0 100000.0 0.0)))
  (make-instance '<gl-light>
                 :id id
                 :ambient ambient
                 :diffuse diffuse
                 :specular specular
                 :position position))
  

(defmethod gl-light-id ((light <gl-light>))
  (+ clyax:GL_LIGHT0 (id-of light)))

(defmethod setup-gl-light ((light <gl-light>))
  (let ((light-id (gl-light-id light)))
    (gl-light-fv light-id clyax:GL_AMBIENT (ambient-of light))
    (gl-light-fv light-id clyax:GL_SPECULAR (specular-of light))
    (gl-light-fv light-id clyax:GL_POSITION (position-of light))
    (gl-light-fv light-id clyax:GL_DIFFUSE (diffuse-of light))
    (clyax:glEnable light-id)
    light-id))

(defmethod enable-gl-light ((light <gl-light>))
  (clyax:glEnable (gl-light-id light)))

