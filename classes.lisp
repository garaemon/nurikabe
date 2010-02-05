;;================================================
;; classes.lisp
;;
;; class.lisp defines all of the nurikabe classes.
;;
;; -------------------------------------------------
;; 'is-a' relation
;; 
;; <manager>
;; <image>
;; <window> 
;;    +----<widget>
;;            +----<button-widget>
;;            |            +----<toggle-button-widget>
;;            |            +----<click-button-widget>
;;            +----<canvas>
;;                     +----<gl-canvas>
;; <packing-box>
;;    +----<vertical-packing-box>
;;    +----<horizontal-packing-box>
;;
;; -------------------------------------------------
;; 'has-a' relation
;;
;; <manager>----------+
;;     |              |
;; <window>--<image>  |
;;     |              |
;; <widget>-----------+
;;     |
;; <image>
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package #:nurikabe)

(defclass* <manager>
    ()
  ((windows nil)                  ;the list of <window>
   (widgets nil)                  ;the list of <widget>
   (display nil)                  ;xwindow's display
   (root-screen nil)              ;xwindow's root screen
   (root-window nil)              ;xwindow's root window
   (logger nil)                   ;an instance of chimi:<logger>
   (loggingp nil)                 ;logging or not
   (current-gl-context nil)       ;for gl:makecrrent
   (gl-textures 0)                ;for substituing for glGenTexture
   (event-thread nil)             ;subthread which runs event-loop
   (thread-hooks nil)             ;event-thread hook
   (xevent nil)                   ;c object of xevent for reducing allocation
   (mutex (make-mutex))))         ;mutex for event-loop in subthread

;; window-core
;;     +---> window
;;     +---> widget
;;              +---> image-widget
;;                        +---> text-widget
;;                        +---> button-widget

(defclass* <window-core>
    ()
  ((width nil) (height nil)             ;size of window
   (name "")                            ;title of window
   (gcontext nil)                       ;graphics context
   (x nil) (y nil)                      ;position of window
   (manager nil)                        ;reference to manager
   (background nil)                     ;background color
   (xwindow nil))                       ;for x window
  (:documentation
   "superclass of <window> and <widget>"))

(defclass* <window>
    (<window-core>)
  ((widgets nil))                       ;the list of <widgets>
  (:documentation
   "<window> class is a container of <widget>.
any contents in <window> is must be realized through <widget>."))

(defclass* <image>
    ()
  ((width nil)                          ;width of image
   (height nil)                         ;height of image
   (depth nil)                          ;depth of image. default is ...
   (content nil)                        ;3A array
   (foreground nil)                     ;default foreground color
   (background nil)                     ;default background color
   (font-loader nil)))                  ;font manager

;; <packing-box> classes
(defclass* <packing-box>
    ()
  ((boxes nil)                          ;
   (window nil)                         ;
   (root-packing-box nil)               ;
   (widgets nil)))                      ;widgets inside the packing-box

(defclass* <vertical-packing-box>
    (<packing-box>)
  ())

(defclass* <horizontal-packing-box>
    (<packing-box>)
  ())

;; <widget> classes
(defclass* <widget>
    (<window-core>)
  ((parent nil)                         ;parent window or widget
   (window nil)                         ;relation to <window>
   (debugp nil))                        ;debugging flag
  )

(defclass* <image-widget>
    (<widget>)
  ((image nil)                          ;an instance of <image>
   (ximage nil)                         ;a c object
   (image-array nil)                    ;c content of ximage
   )
  )

(defclass* <button-widget>
    (<image-widget>)
  ((button-state nil)
   (button-size 0.5d0)
   (button-region nil)) ;(left-up-x left-up-y right-down-x right-down-y)
  )

(defclass* <toggle-button-widget>
    (<button-widget>)
  ((display-string "")                  ;
   (display-string-offset nil)          ;
   (font-size nil))                     ;
  (:documentation
   "widget class for a toggle button.
    +-------------+
    |<--> offset  |
    | x    hoge   |
    +-------------+
   "))

(defclass* <click-button-widget>
    (<button-widget>)
  ((display-string "")                  ;string of button
   (button-callback nil)                ;callback function
   (button-color :skyblue)              ;button color
   (pressed-color :blue)
   (string-draw-point nil)              ;(x y)
   (button-font-size 10))               ;font size of display-text
  (:documentation
   "just a simple button.

   When button pressed, call press-callback function.")
  )

(defclass* <canvas>
    (<widget>)
  ()
  (:documentation
   "<canvas> class is a ..."))

(defclass* <gl-canvas>
    (<canvas>)
  ((gl-context nil)                     ;
   (default-gl-line-width 2.0))         ;
  (:documentation
   ""))

(defclass* <gl-window>
    (<window>)
  ((gl-context nil)                     ;
   (default-gl-line-width 2.0))         ;
  (:documentation
   ""))
