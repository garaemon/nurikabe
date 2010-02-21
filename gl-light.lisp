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
                 :id id :ambient ambient :diffuse diffuse
                 :specular specular :position position))

(defmethod gl-light-id ((light <gl-light>))
  (+ gl:+light0+ (id-of light)))

(defmethod enable ((light <gl-light>))
  (let ((light-id (gl-light-id light)))
    (gl:light-fv light-id gl:+ambient+ (ambient-of light))
    (gl:light-fv light-id gl:+specular+ (specular-of light))
    (gl:light-fv light-id gl:+position+ (position-of light))
    (gl:light-fv light-id gl:+diffuse+ (diffuse-of light))
    (gl:enable light-id)
    light-id))

(defmethod disable ((light <gl-light>))
  (gl:disable (gl-light-id light)))
