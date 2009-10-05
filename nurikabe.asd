(defsystem nurikabe
    :depends-on (chimi clx cl-vectors
		 cl-aa-misc cl-vectors zpb-ttf
		 cl-paths-ttf iterate
                 cffi
                 clyax
                 nurarihyon
                 alexandria)
    :components ((:file "nurikabe")
		 (:file "classes"
			:depends-on ("nurikabe"))
		 (:file "gl-wrapper"
			:depends-on ("nurikabe"))
		 (:file "font"
			:depends-on ("nurikabe"))
		 (:file "color"
                        :depends-on ("nurikabe"))		 
		 (:file "manager"
                        :depends-on ("nurikabe" "classes"))
		 (:file "window"
                        :depends-on ("nurikabe" "classes" "manager" "color"))
		 (:file "image"
                        :depends-on ("nurikabe" "classes" "manager" "color"))
		 (:file "canvas"
                        :depends-on ("nurikabe" "classes" "manager" "color"))
		 (:file "gl-canvas"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "canvas" "gl-wrapper"))
                 (:file "gl-window"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "canvas" "gl-wrapper"))
                 (:file "gl-light"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "canvas" "gl-wrapper"))
                 (:file "gl-material"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "canvas" "gl-wrapper"))
		 (:file "widget"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "window"))
		 (:file "toggle-button-widget"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "window" "widget"))
		 (:file "click-button-widget"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "window" "widget"))
		 (:file "packing-box"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "window"))
		 (:file "viewer"
                        :depends-on ("nurikabe" "classes" "manager" "color"
                                     "window" "gl-wrapper" "gl-canvas"))
		 ))
