;;================================================
;; geometry.lisp
;;
;; Geometric relations of widgets is managed by
;; <geometry> class and its subclasses.
;; If you want to 'resizable' widget and window,
;; you have to use <geometry> class.
;;
;; <window> and <container-widgets> can have some <geometry>s
;; for arrangements of widgets.
;;
;; You need to make <geometry> AFTER making parent <window> or
;; <container-widget>.
;; 
;; You can make a <geometry> inside of some <geometry>,
;; because nurikabe supports nested <geometry>.
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurikabe)

;; verticalとhorizontalにわけてるけど, これでうまくいくんだろうか?
(defclass* <geometry>
    ()
  ((widgets nil)
   (parent nil)
   (vertical-policy nil)
   (horizontal-policy nil))
  (:documentation
   "<geometry> is consist from the vertical and horizontal policies."))

(defclass* <geometry-policy>
    ()
  ())

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *geometry-policies* nil))

(defmacro defgeometry-policy (policy-name keyword-alias &rest clos-args)
  `(progn
     ;; define class
     (defclass* ,policy-name ,@clos-args)
     (eval-when (:compile-toplevel :load-toplevel)
       (unless (assoc ,keyword-alias *geometry-policies*)
         (push (cons ,keyword-alias ',policy-name) *geometry-policies*)))
     )
  )

(defgeometry-policy <center-policy> :center
  (<geometry-policy>)
  ())

(defgeometry-policy <upper-policy> :upper
    (<geometry-policy>)
  ())

(defgeometry-policy <lower-policy> :lower
    (<geometry-policy>)
  ())

(defgeometry-policy <fix-policy> :fix
    (<geometry-policy>)
  ((position 0)))

(defgeometry-policy <fix-center-policy> :fix-center
  (<fix-policy>)
  ())

(defun make-geometry-policy (key/class &rest args)
  (cond ((keywordp key/class)
         (apply #'make-instance (chimi:assoc-ref key/class *geometry-policies*)
                :allow-other-keys t
                args))
        ((symbolp key/class)
         (apply #'make-instance key/class :allow-other-keys t args))
        ((listp key/class)
         (apply #'make-geometry-policy (append key/class args)))
        (t key/class)))

(defun make-geometry (&rest args
                      &key
                      (vertical nil)
                      (horizontal nil)
                      (widgets nil)
                      (parent nil)
                      &allow-other-keys)
  "make a geometry from vertical and horizontal policy."
  (if (null (and vertical horizontal))
      (error "You have to specify :vertical and :horizontal keyword"))
  (let ((geo (apply #'make-instance '<geometry>
                    :vertical-policy (make-geometry-policy vertical)
                    :horizontal-policy (make-geometry-policy horizontal)
                    :widgets widgets
                    :allow-other-keys t args)))
    ;; add geo to parent
    (add-geometry parent geo)
    (arrange-widgets geo)
    geo))

(defmethod add-widget ((geo <geometry>) (widget <widget>))
  "add widget to geometry at the tail of widgets list."
  (with-slots (widgets) geo
    (setf widgets (append widgets (list widget)))))

(defmethod add-widgets ((geo <geometry>) wds)
  "add widgets to geometry at the tail of widgets list."
  (with-slots (widgets) geo
    (setf widgets (append widgets wds))))

(defmethod arrange-widgets ((geo <geometry>))
  "Arrange positions of widgets in geometry.
In this method, <geometry> calls arrange-widgets-policy method
 of vertical and horizontal policy."
  (with-slots (widgets vertical-policy horizontal-policy parent) geo
    (when widgets
      (with-slots (width height) parent
        (let ((xs (arrange-widgets-policy horizontal-policy
                                          (mapcar #'width-of widgets)
                                          width))
              (ys (arrange-widgets-policy vertical-policy
                                          (mapcar #'height-of widgets)
                                          height)))
          (mapcar #'(lambda (w x y) (move w x y)) widgets xs ys)
          ;; need flush?
          t)))))

(defmethod arrange-widgets-policy ((policy <geometry-policy>) widgets width)
  "this is a virtual method.
You need to over load this method in subclass of <geometry-policy>.
arrange-widgets-policy returns the list of position of widgets."
  t)

(defmethod arrange-widgets-policy ((policy <upper-policy>) widgets-width width)
  "set the widgets from 0 to width..."
  (iterate:iter
    (iterate:for w in widgets-width)
    (iterate:for prev-w previous w initially 0)
    (iterate:sum prev-w into prev-width)
    ;; need to check over width?
    (iterate:collect prev-width)))

(defmethod arrange-widgets-policy ((policy <lower-policy>) widgets-width width)
  "set the widgets from width to 0"
  (iterate:iter
    (iterate:for w in widgets-width)
    (iterate:sum w into prev-width)
    (iterate:collect (- width prev-width))))

(defmethod arrange-widgets-policy ((policy <center-policy>)
                                   widgets-width width)
  "centering widgets"
  (let ((widgets-with-sum (apply #'+ widgets-width))
        (widgets-num (length widgets-width)))
    (let ((rest-width (- width widgets-with-sum)))
      (if (< rest-width 0) (error "too large widgets")) ;need check?
      (let ((space (round (/ rest-width (1+ widgets-num)))))
        (iterate:iter
          (iterate:for w in widgets-width)
          (iterate:for prev-w previous w initially 0)
          (iterate:sum (+ prev-w space) into prev)
          (iterate:collect prev))))))

(defmethod arrange-widgets-policy ((policy <fix-policy>)
                                   widgets-width width)
  (make-list (length widgets-width) :initial-element (position-of policy)))

(defmethod arrange-widgets-policy ((policy <fix-center-policy>)
                                   widgets-width width)
  (with-slots (position) policy
    ;; calc mean width
    (let ((mean-width (round (nh:mean widgets-width))))
      (let ((rest-width (- width mean-width)))
        (setf position (round (/ rest-width 2))))))
  (call-next-method))
  
