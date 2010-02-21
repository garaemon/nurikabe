(defsystem nurikabe
    :depends-on (chimi cl-vectors
		 cl-aa-misc cl-vectors zpb-ttf cl-paths-ttf iterate
                 cffi clyax nurarihyon alexandria trivial-garbage cl-wand)
    :version "0.1"
    :components ((:file "nurikabe")
 		 (:file "font" :depends-on ("nurikabe"))
		 (:file "color" :depends-on ("nurikabe"))
		 (:file "manager" :depends-on ("nurikabe"))
		 (:file "window" :depends-on ("manager" "color"))
                 (:file "widget"
                        :depends-on ("manager" "color" "window"))
		 (:file "image"
                        :depends-on ("manager" "color" "font"))
                 (:file "image-widget"
                        :depends-on ("nurikabe"  "manager" "color"
                                      "window" "image" "widget"))
                 (:file "container-widget"
                        :depends-on ("nurikabe"  "manager" "widget"
                                      "window"))
                 (:file "button-widget" :depends-on ("image-widget"))
                 (:file "click-button-widget" :depends-on ("button-widget"))
                 (:file "toggle-button-widget" :depends-on ("button-widget"))
                 (:file "image-viewer-widget" :depends-on ("image-widget" ))
                 (:file "slide-widget" :depends-on ("button-widget" ))
                 ;; for gl
                 (:file "gl-light")
                 (:file "gl-material")
                 (:file "gl-widget"
                        :depends-on ("gl-light" "gl-material" "widget"))
;;                  (:file "gl-window"
;;                         :depends-on ("nurikabe" "classes" "manager" "color"
;;                                      "canvas"))
;;                  (:file "gl-light"
;;                         :depends-on ("nurikabe" "classes" "manager" "color"
;;                                      "canvas"))
;;                  (:file "gl-material"
;;                         :depends-on ("nurikabe" "classes" "manager" "color"
;;                                      "canvas"))
;; 		 (:file "toggle-button-widget"
;;                         :depends-on ("nurikabe" "classes" "manager" "color"
;;                                      "window" "widget"))
;; 		 (:file "click-button-widget"
;;                         :depends-on ("nurikabe" "classes" "manager" "color"
;;                                      "window" "widget"))
;; 		 (:file "packing-box"
;;                         :depends-on ("nurikabe" "classes" "manager" "color"
;;                                      "window"))
		 ))
