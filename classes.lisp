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
  ((windows nil)                  ;the list of <window>( and <widget>)
   (display nil)                  ;xwindow's display
   (root-screen nil)              ;xwindow's root screen
   (root-window nil)              ;xwindow's root window
   (logger nil)                   ;an instance of chimi:<logger>
   (loggingp nil)                 ;logging or not
   (current-gl-context nil)       ;for gl:makecrrent
   (gl-textures 0)                ;for substituing for glGenTexture
   (event-thread nil)             ;subthread which runs event-loop
   (mutex (make-mutex))))         ;mutex for event-loop in subthread

(defclass* <window>
    ()
  ((width nil)                     ;width of window
   (height nil)                    ;height of window
   (name "")                       ;title of window
   (image nil)                     ;instance of <image>
   (image-array nil)               ;simple array of (unsigned 8). #1A.
   (gcontext nil)                  ;graphics context
   (manager nil)                   ;reference to manager
   (xwindow nil)                   ;for x window
   (x nil)                         ;x position in display
   (y nil)                         ;y position in display
   (widgets nil)                   ;the list of <widgets>
   ))

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
    (<window>)
  ((parent nil)                         ;
   (debugp nil))                        ;
  (:documentation
   "There are some limitations in <widget>
    
    -- widget must be a rectangle."
   ))

(defclass* <button-widget>
    (<widget>)
  ((button-state nil)                   ;
   (button-region nil))                 ;
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
  ((display-string "")                  ;
   (press-callback nil)                 ;
   (display-string-margin nil)          ;
   (font-size nil))                     ;
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
