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

;; <manager>のインスタンスは必ずこの*manager*
;; にバインドされる
(defvar *manager* nil)

;; nurikabeの初期化関数. XWindowをオープンして,
;; <manager>のインスタンスを作る.
(defun init-gui (&key (loggingp nil) (threadingp t))
  "Initialize function for NURIKABE package.
   This function must be called before any nurikabe process.

   When it called, <manager>'s instance, *manager* is created
   and open X11 display."
  (unless *manager*
    (setf *manager* (make-instance '<manager> :loggingp loggingp))
    (setf (display-of *manager*) (clyax:XOpenDisplay ""))
    (setf (root-window-of *manager*)
          (clyax:XDefaultRootWindow (display-of *manager*)))
    (setf (root-screen-of *manager*)
          (clyax:XDefaultScreen (display-of *manager*)))
    (setf (logger-of *manager*)
	  (chimi:make-logger :location "/tmp/nurikabe.log"))
    (setf (xevent-of *manager*)
          (cffi:foreign-alloc 'clyax::XEvent))
    (when threadingp
      (setf (event-thread-of *manager*)
            (chimi:make-thread
             (lambda ()
               (clyax:Xsync (display-of *manager*) 1)
               (while t (event-loop *manager*))))))
    )
  *manager*)

;; xlibパッケージのwindowをnurikabeパッケージ<window>へ
;; 変換する.
(defmethod xlib-window->window ((manager <manager>) win)
  "convert window of xwindow to <window> instance."
  (find (cffi:pointer-address win) (windows-of manager)
        :key #'(lambda (x) (cffi:pointer-address (xwindow-of x)))))

(defmacro with-xlib-window (params &rest bodies)
  "exec bodies if the window exists.

  params a list like (symbol xwindow manager)"
  (let ((xwin (gensym))
        (manager (gensym)))
    `(let* ((,manager ,(caddr params))
            (,xwin (xlib-window->window ,manager ,(cadr params))))
       (let ((,(car params) ,xwin))
           (if ,xwin
               (progn ,@bodies))))))

;; XWindowをflushする.
(defmethod flush ((man <manager>))
  "Flush XWindow output."
  (clyax:XFlush (display-of man)))

;; managerにログを書き込む.
(defmethod log-format ((manager <manager>) str &rest args)
  (if (loggingp-of manager)
      (apply #'log-format (logger-of manager) str args)
      t))
  
(defmacro with-x-serialize ((manager) &rest args)
  `(chimi:with-mutex ((mutex-of ,manager))
     ,@args))

;; このメソッドはsubthreadで周期的に呼ばれる.
;; 各種イベントに対して適切なcallbackを呼び出す.
;; callbackは(mutex-of manager)がかかった状態で呼び
;; 出されることに注意.
;; イベントを処理した後に全てのwidgetを描画し直す.
(defmethod event-loop ((manager <manager>))
  (let ((event (xevent-of manager)))
    (with-x-serialize (manager)
      (while (> (clyax:XEventsQueued (display-of manager) 1) 0)
        (clyax::XNextEvent (display-of manager) event)
        (let ((type (foreign-slot-value event 'clyax::XEvent 'clyax::type)))
          (cond
            ((= clyax::Expose type)
             (with-foreign-slots
                 ((clyax::x clyax::y
                            clyax::window
                            clyax::width clyax::height
                            clyax::count)
                  (foreign-slot-value event
                                      'clyax::XEvent
                                      'clyax::xexpose)
                  clyax::XExposeEvent)
               (with-xlib-window
                   (win clyax::window manager)
                 (log-format manager ":exposure event to ~A" win)
                 (exposure-callback win
                                    clyax::x
                                    clyax::y
                                    clyax::width
                                    clyax::height
                                    clyax::count))))
            ((= clyax::ResizeRequest type)
             (with-foreign-slots
                 ((clyax::width clyax::height clyax::window)
                  (foreign-slot-value event
                                      'clyax::XEvent
                                      'clyax::xresizerequest)
                  clyax::XResizeRequestEvent)
               (with-xlib-window
                   (win clyax::window manager)
                 (log-format manager ":resize event to ~A" win)
                 (resize-callback win clyax::width clyax::height))))
            ((= clyax::MotionNotify type)
             (with-foreign-slots
                 ((clyax::x clyax::y clyax::window
                            clyax::x_root clyax::y_root)
                  (foreign-slot-value event
                                      'clyax::XEvent
                                      'clyax::xmotion)
                  clyax::XMotionEvent)
               (with-xlib-window
                   (win clyax::window manager)
                 (log-format manager ":motion-notify event to ~A" win)
                 (motion-notify-callback win clyax::x clyax::y nil))))
            ((= clyax::ButtonPress type)
             (with-foreign-slots
                 ((clyax::x clyax::y clyax::window
                            clyax::x_root clyax::y_root)
                  (foreign-slot-value event
                                      'clyax::XEvent
                                      'clyax::xbutton)
                  clyax::XButtonEvent)
               (with-xlib-window
                   (win clyax::window manager)
                 (log-format manager ":button-press event to ~A" win)
                 (button-press-callback win clyax::x clyax::y))))
            ((= clyax::ButtonRelease type)
             (with-foreign-slots
                 ((clyax::x clyax::y clyax::window
                            clyax::x_root clyax::y_root)
                  (foreign-slot-value event
                                      'clyax::XEvent
                                      'clyax::xbutton)
                  clyax::XButtonEvent)
               (with-xlib-window
                   (win clyax::window manager)
                 (log-format manager ":button-release event to ~A" win)
                 (button-release-callback win clyax::x clyax::y))))
            ((= clyax::ConfigureNotify type)
             (with-foreign-slots
                 ((clyax::x clyax::y clyax::window
                            clyax::width clyax::height)
                  (foreign-slot-value event
                                      'clyax::XEvent
                                      'clyax::xconfigure)
                  clyax::XConfigureEvent)
               (with-xlib-window
                   (win clyax::window manager)
                 (log-format manager ":configure-notify event to ~A" win)
                 (configure-notify-callback win
                                            clyax::x clyax::y
                                            clyax::width clyax::height))))
            ((= clyax::EnterNotify type)
             (log-format manager ":enter-notify event")
             )
            (t
             )))))
    (iterate:iter (iterate:for win in
                               (remove-if
                                #'(lambda (w) (subtypep (class-of w) '<widget>))
                                (windows-of manager)))
                  (render-widgets win)
                  (flush-window win :clear nil)
                  )
    ;;(flush manager)
    (sleep 0.01)                        ;sleep for not monopoly thread
    ))

;; managerに<window>を追加する.
;; 全ての<window>は<manager>によって管理される必要がある.
;; これはevent-loopを適切に処理するためである.
;; これは<window>のサブクラスである<widget>についても同様である.
(defmethod add-window ((manager <manager>) (window <window>))
  (push window (windows-of manager))
  (setf (manager-of window) manager)
  window)

;; display-finish-outputを呼び出す.
;; 短い周期で繰り返し呼ぶと, xserverとの接続が
;; 不安定になるので注意が必要.
(defun xflush ()
  (clyax:XFlush (display-of *manager*)))

;; nurikabeで共通に使われるeventのマスクを
;; 返す
(defun default-event-mask ()
  (logior
   clyax:ExposureMask
   clyax:ButtonPressMask
   clyax:ButtonReleaseMask
   clyax:Button1MotionMask
   clyax:StructureNotifyMask
   clyax:SubstructureNotifyMask))

(defun default-attribute-mask ()
  (logior
   clyax:CWEventMask
   clyax:CWColormap
   clyax:CWBackPixmap
   ))

(defun new-texture-name (&optional (man *manager*))
  (incf (gl-textures-of man)))

(defmethod wait-event ((manager <manager>) ev)
  "wait until event will ..."
  (with-foreign-object
      (event 'clyax::XEvent)
    (while t
      (while (> (clyax:XEventsQueued (display-of manager) 1) 0)
        (clyax::XNextEvent (display-of manager) event)
        (let ((type (foreign-slot-value event 'clyax::XEvent 'clyax::type)))
          (if (eq type ev)
              (return-from wait-event t))))
      (sleep 0.01)
      )))
    
