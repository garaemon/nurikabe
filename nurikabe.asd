(defsystem nurikabe
    :depends-on (chimi clx cl-vectors
		 cl-aa-misc cl-vectors zpb-ttf
		 cl-paths-ttf iterate)
    :components
    ((:file "gui")
;;      (:file "classes"
;;             :depends-on ("gui"))
;;      (:file "manager"
;;             :depends-on ("gui" "classes"))
;;      (:file "window"
;;             :depends-on ("gui" "classes" "manager"))
;;      (:file "image"
;;             :depends-on ("gui" "classes" "window"))
;;      (:file "color"
;;             :depends-on ("gui"))
;;      (:file "widget"
;;             :depends-on ("gui" "classes" "window" "manager"))
;;      (:file "toggle-button-widget"
;;             :depends-on ("gui" "classes" "window" "manager" "widget"))
;;      (:file "click-button-widget"
;;             :depends-on ("gui" "classes" "window" "manager" "widget"))
;;      (:file "canvas" :depends-on
;;             ("gui" "classes" "window" "manager" "widget"))
;;      (:file "gl-canvas" :depends-on
;;             ("gui" "classes" "window" "manager" "widget" "canvas"))
;;      (:file "packing-box"
;;             :depends-on ("gui" "classes" "window" "manager" "widget"))
;;      (:file "viewer"
;;             :depends-on ("gl-canvas" "packing-box"))
;;      (:file "gl-wrapper")
     ))
