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
               (xlib:sync :display (display-of *manager*)
                          :discardp t)
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
  (xlib:flush :display (display-of man)))

;; managerにログを書き込む.
(defmethod log-format ((manager <manager>) str &rest args)
  (if (loggingp-of manager)
      (apply #'log-format (logger-of manager) str args)
      t))

(defmacro with-x-serialize ((manager) &rest args)
  `(chimi:with-mutex ((mutex-of ,manager))
     ,@args))

(defmethod add-thread-hook ((manager <manager>) function)
  (with-x-serialize (manager)
    (push function (thread-hooks-of manager))))

;; このメソッドはsubthreadで周期的に呼ばれる.
;; 各種イベントに対して適切なcallbackを呼び出す.
;; callbackは(mutex-of manager)がかかった状態で呼び
;; 出されることに注意.
;; イベントを処理した後に全てのwidgetを描画し直す.
(defmethod event-loop ((manager <manager>))
  (let ((event (xevent-of manager)))
    (with-x-serialize (manager)
      (while (> (xlib:events-queued :display (display-of manager)
                                    :mode 1) ;1??
                0)
        (xlib:next-event :display (display-of manager) :event event)
        (xlib:event-case
         event
         (xlib:+expose+
          (xlib::x xlib::y xlib::count xlib::window xlib::width xlib::height)
          (with-xlib-window
              (win xlib::window manager)
            (log-format manager ":exposure event to ~A" win)
            (exposure-callback win xlib::x xlib::y
                               xlib::width xlib::height
                               xlib::count)))
         (xlib:+resize-request+
          (xlib::width xlib::height xlib::window)
          (with-xlib-window
              (win xlib::window manager)
            (log-format manager ":resize event to ~A" win)
            (resize-callback win xlib::width xlib::height)))
         (xlib:+motion-notify+
          (xlib::x xlib::y xlib::window
           xlib::x_root xlib::y_root)
          (with-xlib-window
              (win xlib::window manager)
            (log-format manager ":motion-notify event to ~A" win)
            (motion-notify-callback win xlib::x xlib::y nil)))
         (xlib:+button-press+
          (xlib::x xlib::y xlib::window
           xlib::x_root xlib::y_root)
          (with-xlib-window
              (win xlib::window manager)
            (log-format manager ":button-press event to ~A" win)
            (button-press-callback win xlib::x xlib::y)))
         (xlib:+button-release+
          (xlib::x xlib::y xlib::window
           xlib::x_root xlib::y_root)
          (with-xlib-window
              (win xlib::window manager)
            (log-format manager ":button-release event to ~A" win)
            (button-release-callback win xlib::x xlib::y)))
         (xlib:+configure-notify+
          (xlib::x xlib::y xlib::window
           xlib::width xlib::height)
          (with-xlib-window
              (win xlib::window manager)
            (log-format manager ":configure-notify event to ~A" win)
            (configure-notify-callback win
                                       xlib::x xlib::y
                                       xlib::width xlib::height)))
         (xlib:+enter-notify+
          ()
          (log-format manager ":enter-notify event"))
         )
        )
      (iterate:iter
        (iterate:for f in (thread-hooks-of manager))
        (funcall f))
      )
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

(defun xflush ()
  (xlib:flush :display (display-of *manager*)))

;; nurikabeで共通に使われるeventのマスクを
;; 返す
(defun default-event-mask ()
  (logior
   xlib:+exposure-mask+
   xlib:+button-press-mask+
   xlib:+button-release-mask+
   xlib:+button1-motion-mask+
   xlib:+substructure-notify-mask+
   xlib:+structure-notify-mask+))

(defun default-attribute-mask ()
  (logior
   xlib:+cw-event-mask+
   xlib:+cw-colormap+
   xlib:+cw-back-pixmap+))

(defun new-texture-name (&optional (man *manager*))
  (incf (gl-textures-of man)))

(defmethod wait-event ((manager <manager>) ev)
  "wait until event will ..."
  (with-foreign-object
      (event 'xlib::XEvent)             ;どうすべきか?
    (while t
      (while (> (xlib:events-queued :display (display-of manager)
                                    :mode 1)
                0)
        (xlib:next-event :display (display-of manager) :event event)
        (let ((type (xlib:event-type event)))
          (if (eq type ev)
              (return-from wait-event t))))
      (sleep 0.01))))
