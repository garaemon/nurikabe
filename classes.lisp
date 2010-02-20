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


;; window-core
;;     +---> window
;;     +---> widget
;;              +---> image-widget
;;                        +---> text-widget
;;                        +---> button-widget



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









