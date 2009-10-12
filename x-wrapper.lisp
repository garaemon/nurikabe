;;================================================
;; x-wrapper.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
                   (safety 3)))


(in-package :nurikabe)

(defun x-create-window (&key
                        (display nil)
                        (parent nil)
                        (screen nil)
                        (x nil) (y nil)
                        (depth 24)
                        (width nil) (height nil)
                        (event-mask (default-event-mask))
                        (border-width 2))
  ;; check arguments
  (if (null parent)
      (error "You have to set :parent"))
  (if (null display)
      (error "You have to set :display"))
    (if (null screen)
      (error "You have to set :screen"))
  (if (or (null x) (null y))
      (error "You have to set :x and :y"))
  (if (or (null height) (null width))
      (error "You have to set :width and :height"))
  (with-foreign-objects
      ((vi 'clyax::XVisualInfo)
       (xattr 'clyax::XSetWindowAttributes))
    ;; setup visual
    (clyax::XMatchVisualInfo display
                             screen
                             depth
                             clyax::TrueColor
                             vi)
    (setf (foreign-slot-value xattr
                              'clyax::XSetWindowAttributes
                              'clyax::event_mask)
          event-mask)
    ;; VisualInfo -> Visual
    (let ((vis (foreign-slot-value vi
                                   'clyax::XVisualInfo
                                   'clyax::visual)))
      ;; colormap
      (setf (foreign-slot-value xattr
                                'clyax::XSetWindowAttributes
                                'clyax::colormap)
            (clyax::XCreateColormap display
                                    parent
                                    vis
                                    clyax::AllocNone))
      (setf (foreign-slot-value xattr
                                    'clyax::XSetWindowAttributes
                                    'clyax::override_redirect)
            1)
      (let ((ret (clyax::XCreateWindow
                  display
                  parent
                  x y                   ;x, y
                  width height          ;width, height
                  border-width
                  clyax::CopyFromParent ;depth
                  clyax::InputOutput    ;class
                  vis
                  (default-attribute-mask) ;attrib mask
                  xattr)))
        (clyax::XSelectInput display
                             ret
                             event-mask)
        ret))))
