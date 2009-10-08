;;====================================================================
;; gl-wrapper.lisp
;;
;; 良い感じにwrappする
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

;; gl wrapper
(in-package :nurikabe)

(defmacro gl-push-pop-matrix ( &rest args)
  `(progn
     (clyax:glPushMatrix)
     (prog1
         (progn ,@args)
       (clyax:glPopMatrix))))

(defmacro gl-push-pop-attribute ( &rest args)
  `(progn
     (clyax:glPushAttrib clyax:GL_ALL_ATTRIB_BITS)
     (prog1
         (progn ,@args)
       (clyax:glPopAttrib))))

(defmacro gl-disable-block (op &rest args)
  `(progn
     (clyax:glDisable ,op)
     (prog1
         (progn ,@args)
       (clyax:glEnable ,op))))

(defmacro gl-enable-block (op &rest args)
  `(progn
     (clyax:glEnable ,op)
     (prog1
         (progn ,@args)
       (clyax:glDisable ,op))
     ))

(declaim (inline gl-clear-color-fv))
(defun gl-clear-color-fv (v)
  (clyax:glClearColor (elt v 0) (elt v 1) (elt v 2) (elt v 3)))

(declaim (inline gl-vertex-3fv))
(defun gl-vertex-3fv (v)
  (clyax:glVertex3f (elt v 0) (elt v 1) (elt v 2)))

(declaim (inline gl-color-3fv))
(defun gl-color-3fvw (v)
  (clyax:glColor3f (elt v 0) (elt v 1) (elt v 2)))


(declaim (inline gl-translate-fv))
(defun gl-translate-fv (v)
  (clyax:glTranslatef (elt v 0) (elt v 1) (elt v 2)))

(declaim (inline gl-normal-3fv))
(defun gl-normal-3fv (v)
  (clyax:glNormal3f (elt v 0) (elt v 1) (elt v 2)))

(defmacro gl-command (type &rest args)
  `(progn
     (clyax:glBegin ,type)
     ,@args
     (clyax:glEnd)))


(declaim (inline gl-light-fv))
(defun gl-light-fv (op1 op2 v)
  (with-cl-sequence->cffi-array
      (vec v :float)
    (clyax:glLightfv op1 op2 vec)))

(declaim (inline gl-material-fv))
(defun gl-material-fv (op1 op2 v)
  (with-cl-sequence->cffi-array
      (vec v :float)
    (clyax:glMaterialfv op1 op2 vec)))

(declaim (inline gl-mult-matrix-f))
(defun gl-mult-matrix-f (v)
  (with-cl-sequence->cffi-array
      (vec v :float)
    (clyax:glMultMatrixf vec)))

(declaim (inline gl-color-3fv))
(defun gl-color-3fv (v)
  (clyax:glColor3f (elt v 0) (elt v 1) (elt v 2)))

(declaim (inline gl-tex-coord-2fv))
(defun gl-tex-coord-2fv (v)
  (clyax:glTexCoord2f (elt v 0) (elt v 1)))

(defun gl-new-texture-name ()
  (with-foreign-object
      (name :int)
    (clyax:glGenTextures 1 name)
    (cffi:mem-ref name :int)))

(defmacro gl-display-list-block (&rest args)
  (let ((list (gensym)))
    `(progn
       (let ((,list (clyax:glGenLists 1)))
         (clyax:glNewList ,list clyax:GL_COMPILE)
         (progn
           ,@args)
         (clyax:glEndList)
         ,list))))
