(defsystem nurikabe
    :depends-on (chimi cl-vectors
		 cl-aa-misc cl-vectors zpb-ttf cl-paths-ttf iterate
                 cffi clyax nurarihyon alexandria trivial-garbage cl-wand)
    :version "0.1"
    :components ((:module
                  "base"
                  :pathname "."
                  :components
                  ((:file "nurikabe")
                   (:file "font" :depends-on ("nurikabe"))
                   (:file "color" :depends-on ("nurikabe"))
                   (:file "manager" :depends-on ("nurikabe"))
                   (:file "window" :depends-on ("manager" "color"))
                   (:file "image" :depends-on ("manager" "color" "font"))))
                 (:module
                  "gl-base"
                  :pathname "."
                  :depends-on ("base")
                  :components
                  ((:file "gl-light")
                   (:file "gl-material")))
                 (:module
                  "widgets"
                  :pathname "widgets/"
                  :depends-on ("base" "gl-base")
                  :components
                  ((:file "widget")
                   (:file "image-widget" :depends-on ("widget"))
                   (:file "container-widget" :depends-on ("widget"))
                   (:file "button-widget" :depends-on ("widget"))
                   (:file "click-button-widget" :depends-on ("widget"))
                   (:file "toggle-button-widget" :depends-on ("widget"))
                   (:file "image-viewer-widget"
                          :depends-on ("widget" "slide-widget" "geometry"
                                       "container-widget"))
                   (:file "slide-widget"
                          :depends-on ("widget" "geometry"
                                       "container-widget"))
                   (:file "gl-widget" :depends-on ("widget"))
                   (:file "geometry" :depends-on ("widget")))
                  ))
    )
