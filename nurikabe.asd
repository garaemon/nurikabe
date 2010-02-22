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
                 (:file "widget" :depends-on ("manager" "color" "window"))
		 (:file "image" :depends-on ("manager" "color" "font"))
                 (:file "image-widget"
                        :depends-on ("nurikabe"  "manager" "color"
                                      "window" "image" "widget"))
                 (:file "container-widget"
                        :depends-on ("nurikabe"  "manager" "widget"
                                      "window"))
                 (:file "button-widget" :depends-on ("image-widget"
                                                     "nurikabe"))
                 (:file "click-button-widget" :depends-on ("button-widget"
                                                           "nurikabe"))
                 (:file "toggle-button-widget" :depends-on ("button-widget"
                                                            "nurikabe"))
                 (:file "image-viewer-widget" :depends-on ("image-widget"
                                                           "nurikabe"))
                 (:file "slide-widget" :depends-on ("button-widget"
                                                    "nurikabe"))
                 ;; for gl
                 (:file "gl-light" :depends-on ("nurikabe"))
                 (:file "gl-material" :depends-on ("nurikabe"))
                 (:file "gl-widget"
                        :depends-on ("gl-light" "gl-material"
                                     "widget" "nurikabe" "manager"))
                 (:file "geometry" :depends-on ("nurikabe" "widget" "window"))
		 )
    )