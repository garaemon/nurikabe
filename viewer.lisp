(in-package #:nurikabe)

(defclass* <object-viewer>
    ()
  ((window nil)
   (objects nil)
   (gl-canvas nil)
   (draw-ground-p nil)
   (draw-origin-p nil)
   ))

(defclass* <object-viewer-window>
    (<window>)
  ())

(defclass* <object-viewer-gl-canvas>
    (<gl-canvas>)
  ())

(defmethod render-widget ((canvas <object-viewer-gl-canvas>))
  )

(defmethod draw-objects ((viewer <object-viewer>))
  (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
  (if (draw-origin-p-of viewer)
      (draw-origin))
  (if (draw-ground-p-of viewer)
      (draw-ground))
  (glx:swap-buffers)
  )
  
(defmethod draw-objects ((canv <object-viewer-gl-canvas>))
  (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
  (glx:swap-buffers)
  t)

(defmethod init-gl ((canvas <object-viewer-gl-canvas>))
  (glx:make-current                     ;cannot call twice... why?
   (xwindow-of canvas)
   (gl-context-of canvas))
  (gl:ortho 0.0d0 1.0d0 0.0d0 1.0d0 -1.0d0 1.0d0)
  (gl:clear-color 0.0s0 0.0s0 0.0s0 0.0s0)
  )

(defun make-object-viewer (&rest args
                           &key
                           (width 600)
                           (height 600)
                           &allow-other-keys)
  (unless *manager*
    (init-gui))
  (let ((oviewer (make-instance '<object-viewer>
                                :window (make-window
                                         :window-class '<object-viewer-window>
                                         :width width
                                         :height height
                                         :name "object-viewer"))))
    (let ((gl-canvas (make-gl-canvas (window-of oviewer)
                                     :canvas-class '<object-viewer-gl-canvas>
                                     :width width
                                     :height height
                                     :x 0
                                     :y 0)))
      (setf (gl-canvas-of oviewer)
            gl-canvas))
    (map-widgets (window-of oviewer))
    (init-gl (gl-canvas-of oviewer))
    oviewer))

