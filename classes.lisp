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

(defclass* <container-widget>
    (<widget>)
  ((widgets nil)))

(defclass* <image-widget>
    (<widget>)
  ((image nil)                          ;an instance of <image>
   (ximage nil)                         ;a c object
   (image-array nil)                    ;c content of ximage
   ))

(defclass* <button-widget>
    (<image-widget>)
  ((button-state nil)
   (button-region nil)) ;(left-up-x left-up-y right-down-x right-down-y)
  )

(defclass* <toggle-button-widget>
    (<button-widget>)
  ((display-string "")                  ;
   (display-string-offset nil)          ;
   (button-size nil)                    ;
   (button-font-size 10))              ;
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

(defclass* <slide-button-widget>
    (<button-widget>)
  ((direction (nh:double-vector 1 0))
   (button-callback nil)
   ))

(defclass* <slide-slider-widget>
    (<image-widget>)
  ((verticalp nil)
   (slider-width nil)
   (slider-position 0.0d0)              ;between 0.0 and 1.0
   (clicked-position nil)               ;to memorize clicked position
   (clicked-region nil)                 ;to memorize clicked region
   (update-callback nil)
   ))

(defclass* <slide-widget>
    (<container-widget>)
  ((position 0.0)
   (forward-button-widget nil)
   (backward-button-widget nil)
   (slider-widget nil)
   (verticalp nil)
   )
  )

(defclass* <horizontal-bar-widget>
    (<bar-widget>)
  ())

(defclass* <vertical-bar-widget>
    (<bar-widget>)
  ())

(defclass* <image-viewer-widget>
    (<container-widget>)
  ((viewer-widget nil)
   (horizontal-slide-widget nil)
   (vertical-slide-widget nil)
   (left-up-position '(0.0 0.0))
   ))
