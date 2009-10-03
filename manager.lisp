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
(defun init-gui (&key (loggingp nil))
  "Initialize function for NURIKABE package.
   This function must be called before any nurikabe process.

   When it called, <manager>'s instance, *manager* is created
   and open X11 display."
  (unless *manager*
    (setf *manager* (make-instance '<manager> :loggingp loggingp))
    (setf (display-of *manager*) (xlib:open-display ""))
    (setf (root-screen-of *manager*)
	  (car (xlib:display-roots (display-of *manager*))))
    (setf (root-window-of *manager*)
	  (xlib:screen-root (root-screen-of *manager*)))
    (setf (logger-of *manager*)
	  (chimi:make-logger :location "/tmp/nurikabe.log"))
    (setf (event-thread-of *manager*)
	  (chimi:make-thread
	   (lambda ()
	     (while t (event-loop *manager*)))))
    )
  *manager*)

;; xlibパッケージのwindowをnurikabeパッケージ<window>へ
;; 変換する.
(defmethod xlib-window->window ((manager <manager>) win)
  "convert window of xwindow to <window> instance."
  (find win (windows-of manager) :key #'xwindow-of))

(defmacro with-xlib-window (params &rest bodies)
  "exec bodies if the window exists.

  params a list like (symbol xwindow manager)"
  (let ((xwin (gensym))
        (manager (gensym)))
    `(let* ((,manager ,(caddr params))
            (,xwin (xlib-window->window ,manager ,(cadr params))))
       (let ((,(car params) ,xwin))
           (if ,xwin
               (progn ,@bodies)
               nil)))))

;; XWindowをflushする.
;; xflushよりもこいつを使うべき.
;; xflush(display-finish-output)は短い
;; 周期で呼ぶとXWindowが応答しなくなる.
(defmethod flush ((man <manager>))
  "Flush XWindow output."
  (xlib:display-force-output (display-of man)))

;; managerにログを書き込む.
(defmethod log-format ((manager <manager>) str &rest args)
  (if (loggingp-of manager)
      (apply #'log-format (logger-of manager) str args)
      t))

;; このメソッドはsubthreadで周期的に呼ばれる.
;; 各種イベントに対して適切なcallbackを呼び出す.
;; callbackは(mutex-of manager)がかかった状態で呼び
;; 出されることに注意.
;; イベントを処理した後に全てのwidgetを描画し直す.
(defmethod event-loop ((manager <manager>))
  (xlib:event-case ((display-of manager)
                    :force-output-p nil
                    :discard-p t)
                   (:exposure
                    (window x y width height count)
                    (with-xlib-window (win window manager)
                      (chimi:with-mutex ((mutex-of manager))
                        (log-format manager ":exposure event to ~A" win)
                        (exposure-callback win x y width height count)))
                    t)
                   (:resize-request
                    (window width height)
                    (with-xlib-window (win window manager)
                      (chimi:with-mutex ((mutex-of manager))
                        (log-format manager ":resize-request event to ~A" win)
                        (resize-callback win width height)))
                    t)
                   (:enter-notify
                    ()
                    (log-format manager ":enter-notify event")
                    t)
                   (:button-press
                    (window x y)
                    (with-xlib-window (win window manager)
                      (chimi:with-mutex ((mutex-of manager))
                        (log-format manager ":enter-notify event to ~A" win)
                        (button-press-callback win x y)))
                    t)
                   (:button-release
                    (window x y)
                    (with-xlib-window (win window manager)
                      (chimi:with-mutex ((mutex-of manager))
                        (log-format manager ":button-relase event to ~A" win)
                        (button-release-callback win x y)))
                    t)
                   (:configure-notify
                    (window x y width height)
                    (with-xlib-window (win window manager)
                      (chimi:with-mutex ((mutex-of manager))
                        (log-format manager ":configure-notify event to ~A" win)
                        (configure-notify-callback win x y width height)))
                    )
                   (:motion-notify
                    (window x y code)
                    (with-xlib-window (win window manager)
                      (chimi:with-mutex ((mutex-of manager))
                        (log-format manager ":motion-notify event to ~A" win)
                        (motion-notify-callback win x y code))))
                   (otherwise
                    ()
                    t)
                   )
  (iterate:iter (iterate:for win in (remove-if
                                     #'(lambda (w) (subtypep (class-of w) '<widget>))
                                     (windows-of manager)))
                (render-widgets win)
                (flush-window win :clear t))
  (flush manager)
  )

;; managerに<window>を追加する.
;; 全ての<window>は<manager>によって管理される必要がある.
;; これはevent-loopを適切に処理するためである.
;; これは<window>のサブクラスである<widget>についても同様である.
(defmethod add-window ((manager <manager>) (window <window>))
  (chimi:with-mutex ((mutex-of manager))
    (push window (windows-of manager))
    (setf (manager-of window) manager))
  window)

;; display-finish-outputを呼び出す.
;; 短い周期で繰り返し呼ぶと, xserverとの接続が
;; 不安定になるので注意が必要.
(defun xflush ()
  (xlib:display-finish-output (display-of *manager*)))

;; nurikabeで共通に使われるeventのマスクを
;; 返す
(defun default-event-mask ()
  (xlib:make-event-mask :exposure
                        :button-press
                        :button-release
                        :button-1-motion
                        ;;:resize-redirect
                        :structure-notify
                        :substructure-notify))


(defun new-texture-name (&optional (man *manager*))
  (incf (gl-textures-of man)))
