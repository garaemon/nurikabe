;;================================================
;; manager.lisp
;; 
;; this file gives <manager> class.
;; <manager> class is a singleton class and
;; the instance of <manager> is bound to *manager*.
;;
;; the <manager>'s important task is to run
;; event loop background in subthread.
;; 
;; The creation of the instance of <manager> must be
;; achieved by calling nurikabe::init-gui.
;; 
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (debug 3)
		   (safety 3)))

(in-package #:nurikabe)

(defvar *manager* nil
  "an instance of <manager> is always bind to this symbol.")

(defun init-gui (&key (loggingp nil) (threadingp t))
  "Initialize function for NURIKABE package.
   This function must be called before any nurikabe process.

   When it called, <manager>'s instance, *manager* is created
   and open X11 display."
  (unless *manager*
    (setf *manager* (make-instance '<manager> :loggingp loggingp))
    (setf (display-of *manager*) (xlib:open-display :host ""))
    (setf (root-window-of *manager*)
          (xlib:default-root-window :display (display-of *manager*)))
    (setf (root-screen-of *manager*)
          (xlib:default-screen :display (display-of *manager*)))
    (setf (logger-of *manager*)
	  (chimi:make-logger :location "/tmp/nurikabe.log"))
    (setf (xevent-of *manager*)
          (xlib:new-event))
    (when threadingp
      (setf (event-thread-of *manager*)
            (chimi:make-thread
             (lambda ()
               (xlib:sync :display (display-of *manager*) :discardp t)
               (while t (event-loop *manager*)))))) ;LOOP
    )
  *manager*)

(defmethod xlib-window->window ((manager <manager>) win)
  "convert window of xwindow to <window> instance."
  (find (cffi:pointer-address win)
        (append (windows-of manager) (widgets-of manager))
        :key #'(lambda (x) (cffi:pointer-address (xwindow-of x)))))

(defmacro with-xlib-window ((symbol xwindow manager) &rest bodies)
  "exec bodies if the window exists.

  params a list like (symbol xwindow manager)"
  (let ((xwin (gensym)))
    `(let* ((,xwin (xlib-window->window ,manager ,xwindow)))
       (let ((,symbol ,xwin))
         (if ,xwin
             (progn ,@bodies))))))

;; XWindowをflushする.
(defmethod flush ((man <manager>))
  "Flush XWindow output."
  (xlib:flush :display (display-of man)))

(defmethod log-format ((manager <manager>) str &rest args)
  "write log to manager"
  (if (loggingp-of manager)
      (apply #'log-format (logger-of manager) str args)
      t))

(defmacro with-x-serialize ((manager &key (lock t)) &rest args)
  `(chimi:with-mutex ((mutex-of ,manager) :lock ,lock)
     ,@args))

(defmethod add-thread-hook ((manager <manager>) function)
  "add thread hook to manager.
   function must be a function takes 0 arguments"
  (with-x-serialize (manager)
    (push function (thread-hooks-of manager))))

(defmethod clear-thread-hook ((manager <manager>))
  "clear thread hook of manager"
  (with-x-serialize (manager)
    (setf (thread-hooks-of manager) nil)))

(defmethod has-event-que-p ((manager <manager>))
  (> (xlib:events-queued :display (display-of manager)
                         :mode 1) ;1??
     0))

(defmethod proc-event ((manager <manager>))
  (xlib:next-event :display (display-of manager) :event (xevent-of manager)))

(eval-when (:compile-toplevel)
  ;; apis := (api)
  ;; api := (event win-sym slots bodies ...)
  (defvar *event-callback-apis* nil)
  (defmacro defmanager-event (event win-sym slots &rest bodies)
    `(push (cons ,event (list ',win-sym ',slots ',bodies))
           *event-callback-apis*))
  
  (defmanager-event xlib:+expose+
      win (xlib::x xlib::y xlib::count xlib::window xlib::width xlib::height)
      (exposure-callback win xlib::x xlib::y
                         xlib::width xlib::height
                         xlib::count))
  
  (defmanager-event xlib:+resize-request+
      win (xlib::width xlib::height xlib::window)
      (resize-callback win xlib::width xlib::height))
  
  (defmanager-event xlib:+motion-notify+
      win (xlib::x xlib::y xlib::window xlib::x_root xlib::y_root)
      (motion-notify-callback win xlib::x xlib::y nil))

  (defmanager-event xlib:+button-press+
      win (xlib::x xlib::y xlib::window xlib::x_root xlib::y_root)
      (button-press-callback win xlib::x xlib::y))

  (defmanager-event xlib:+button-release+
      win (xlib::x xlib::y xlib::window xlib::x_root xlib::y_root)
      (button-release-callback win xlib::x xlib::y))
  
  (defmanager-event xlib:+configure-notify+
      win (xlib::x xlib::y xlib::window xlib::width xlib::height)
      (configure-notify-callback win xlib::x xlib::y
                                 xlib::width xlib::height))
  
  (defmanager-event xlib:+enter-notify+
      win ()
      t)

  (defmanager-event xlib:+leave-notify+
      win (xlib::window)
      (leave-notify-callback win)
      t)
  
  
  (defmacro dispatch-and-call-event (manager event)
    `(xlib:event-case
      ,event
      ,@(mapcar #'(lambda (api)
                    `(,(car api)
                       ,(caddr api)
                       (with-xlib-window
                           (,(cadr api) xlib::window ,manager)
                         ;; format to log
                         (log-format ,manager "~s event is occured"
                                     ',(car api))
                         ,@(cadddr api))))
                *event-callback-apis*)))
  )

(defmethod event-loop ((manager <manager>))
  "this method event-loop is called cyclicly in event-thread.
It calls the proper callback methods according to the event.

all of the callback is called in mutex lock.

flow of event-loop is:
1. call callback methods according to the events.
2. call thrad hook functions.
3. call flush window for all of the windows"
  (let ((event (xevent-of manager)))
    (with-x-serialize (manager)
      (while (has-event-que-p manager)  ;for all events
        (proc-event manager)
        (dispatch-and-call-event manager event))
      ;; if it has thread hooks
      (iterate:iter
        (iterate:for f in (thread-hooks-of manager))
        (funcall f)))
    (sleep 0.01)                        ;sleep for not monopoly thread
    ))

(defmethod add-window ((manager <manager>) (window <window>))
  "add a window to manager"
  (push window (windows-of manager))
  (setf (manager-of window) manager)
  window)

(defmethod add-widget ((manager <manager>) (widget <widget>))
  "add a window to manager"
  (push widget (widgets-of manager))
  widget)

(defun xflush ()
  (xlib:flush :display (display-of *manager*)))

;; nurikabeで共通に使われるeventのマスクを
;; 返す
(defun default-event-mask ()
  (logior xlib:+exposure-mask+
          xlib:+button-press-mask+
          xlib:+button-release-mask+
          xlib:+button1-motion-mask+
          xlib:+substructure-notify-mask+
          xlib:+structure-notify-mask+
          xlib:+leave-window-mask+ ))

(defun default-attribute-mask ()
  (logior xlib:+cw-event-mask+ xlib:+cw-colormap+ xlib:+cw-back-pixmap+))

(defun new-texture-name (&optional (man *manager*))
  (incf (gl-textures-of man)))

(defmethod wait-event ((manager <manager>) ev)
  "wait until event will ..."
  (with-foreign-object
      (event 'xlib::XEvent)             ;どうすべきか?
    (while t
      (while (has-event-que-p manager)
        (xlib:next-event :display (display-of manager) :event event)
        (let ((type (xlib:event-type event)))
          (if (eq type ev) (return-from wait-event t))))
      (sleep 0.01))))
