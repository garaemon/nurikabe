(defsystem nurikabe
    :depends-on (chimi cl-vectors
		 cl-aa-misc cl-vectors zpb-ttf cl-paths-ttf iterate
                 cffi clyax nurarihyon alexandria trivial-garbage)
    :version "0.1"
    :components ((:file "nurikabe")
		 (:file "classes"
			:depends-on ("nurikabe"))
 		 (:file "font" :depends-on ("nurikabe"))
		 (:file "color" :depends-on ("nurikabe"))
		 (:file "manager" :depends-on ("nurikabe" "classes"))
		 (:file "window" :depends-on ("classes" "manager" "color"))
                 (:file "widget"
                        :depends-on ("classes" "manager" "color" "window"))
		 (:file "image"
                        :depends-on ("manager" "color" "font"))
                 (:file "image-widget"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                      "window" "image" "widget"))
                 (:file "container-widget"
                        :depends-on ("nurikabe" "classes" "manager" "widget"
                                      "window"))
                 (:file "button-widget" :depends-on ("image-widget"))
                 (:file "click-button-widget" :depends-on ("button-widget"))
                 (:file "toggle-button-widget" :depends-on ("button-widget"))
                 (:file "image-viewer-widget" :depends-on ("image-widget"))
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
