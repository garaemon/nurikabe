;;================================================
;; gl-widget.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurikabe)

(defclass* <gl-widget>
    (<widget>)
  ((gl-contexts nil)                    ;((thread-id . gl-context) ...)
   ))

(defun fill-default-glx-attrib (attrib)
  (setf (mem-aref attrib :int 0) glx:+rgba+)
  (setf (mem-aref attrib :int 1) glx:+red-size+)
  (setf (mem-aref attrib :int 2) 8)
  (setf (mem-aref attrib :int 3) glx:+green-size+)
  (setf (mem-aref attrib :int 4) 8)
  (setf (mem-aref attrib :int 5) glx:+blue-size+)
  (setf (mem-aref attrib :int 6) 8)
  (setf (mem-aref attrib :int 7) glx:+doublebuffer+)
  (setf (mem-aref attrib :int 8) glx:+depth-size+)
  (setf (mem-aref attrib :int 9) 8)
  (setf (mem-aref attrib :int 10) xlib:+none+)
  attrib)

(defmethod current-thread-gl-context ((wid <gl-widget>))
  (cdr (assoc (chimi:current-thread) (gl-contexts-of wid))))

(defmethod need-make-gl-context-p ((wid <gl-widget>))
  (null (current-thread-gl-context wid)))

(defmethod add-gl-context ((wid <gl-widget>) context)
  (with-slots (gl-contexts) wid
    (push (cons (chimi:current-thread) context) gl-contexts)))
  

(defmethod gl-make-current ((wid <gl-widget>))
  (with-slots (manager) wid
    (when (need-make-gl-context-p wid)
      ;; if does not have gl context fot this thread,
      ;; we need to make another gl context fot it.
      (add-gl-context wid (make-gl-context manager)))
    (let ((current-context (current-thread-gl-context wid)))
      )))

;; special widget make function for gl-widget and subclass of it.
(defun make-gl-widget ()
  )

;; gl wrapper
(defmethod make-glx-visual ((manager <manager>))
  (with-foreign-object
      (attrib :int 11)
    (fill-default-glx-attrib attrib)
    (glx:choose-visual :display (display-of manager)
                       :screen (root-screen-of manager)
                       :attribute attrib)))

(defmethod make-gl-context ((manager <manager>)
                            &key (visual (make-glx-visual manager)))
  (glx:create-context :display (display-of manager)
                      :visual-info visual
                      :directp t))
