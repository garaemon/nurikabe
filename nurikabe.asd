(defsystem nurikabe
    :depends-on (chimi clx cl-vectors
		 cl-aa-misc cl-vectors zpb-ttf
		 cl-paths-ttf iterate)
    :components ((:file "nurikabe")
		 (:file "classes" :depends-on ("nurikabe"))
		 (:file "manager" :depends-on ("nurikabe" "classes"))
		 
		 )
    )
