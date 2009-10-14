;;====================================================================
;; image.lisp
;; 
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

(declaim (optimize (debug 3)
		   (safety 3)))

(in-package #:nurikabe)

(defvar *font-paths*
 #+:darwin
 '(#p"/Library/Fonts/"
   #p"/System/Library/Fonts/"
   #p"/usr/X11/lib/X11/fonts/TTF/")
 #+:linux
 '(#p"/usr/share/fonts/truetype/ttf-bitstream-vera/")
 )

;; for printing
(defmethod print-object ((image <image>) stream)
  (print-unreadable-object (image stream :type t :identity t)
    (format stream "~Ax~A"
            (width-of image)
            (height-of image))))

(defun make-image (&key
                   (width nil)
                   (height nil)
                   (depth 3)
                   (content nil)
                   (foreground :black)
                   (background :white)
                   (font "VeraMono.ttf"))
  "When you want to make a image, you have to call this function.
   For, there are many messy settings in making a image.

   <image> class is the most basic class in nurikabe package.
   If you want to show some drawings, you have to access <image> class."
  (let ((ret (make-instance '<image>)))
    ;; set content and width, height
    (cond (content
           (setf (content-of ret) content)
           (setf (height-of ret) (array-dimension ret 0))
           (setf (width-of ret) (array-dimension ret 1)))
          ((and width height)
           (setf (content-of ret)
                 (aa-misc:make-image width height (symbol->rgb-vector background)))
           (setf (width-of ret) width)
           (setf (height-of ret) height))
          (t
           (error "You have to set width,height or content")))
    (setf (depth-of ret) 3)
    (setf (foreground-of ret) foreground) ;initialize foreground color
    (setf (background-of ret) background) ;initialize background color
    (set-font ret font)                   ;initialize font loader
    ret))

(defmethod clear-image ((image <image>))
  (let ((content (content-of image))
        (rgb (symbol->rgb-vector (background-of image))))
    (dotimes (i (height-of image))
      (dotimes (j (width-of image))
        (dotimes (k (depth-of image))
          (setf (aref content i j k) (aref rgb k))))))
  image)

(defmethod copy-image ((image <image>)
                       &optional
                       (target nil))
  (if (null target)
      (setf target (make-image :width (width-of image)
                               :height (height-of image)
                               :depth (depth-of image):content)))
  target)

(defmethod set-font ((image <image>)
                     font-name)
  "I think loading ttf file is a little heavy task.
   So, <image> class has a font-loader slot."
  (when (font-loader-of image)
    (zpb-ttf:close-font-loader (font-loader-of image)))
  (setf (font-loader-of image)
        (zpb-ttf:open-font-loader (find-file-in-path font-name *font-paths*)))
  image)

(defmethod draw-line ((image <image>)
                      from-x from-y
                      to-x to-y
                      &key
                      (color :black))
  "Draw a line from (from-x from-y) to (to-x to-y) in a image.

   You can set the color of line by :color keyword."
  (let ((state (aa:make-state)))
    (aa:line-f state from-x from-y to-x to-y)
    (let* ((put-pixel (aa-misc:image-put-pixel (content-of image)
                                               (symbol->rgb-vector color))))
      (aa:cells-sweep state put-pixel))
    image))

(defmethod draw-polygon ((image <image>) points &key (color :black))
  "draw a polygon.
   The points must be a CCW."
  (let ((state (aa:make-state)))
    (mapcar #'(lambda (from to)
                (aa:line-f state (car from) (cadr from)
                           (car to) (cadr to)))
            points (append (cdr points) (list (car points))))
    (let* ((put-pixel (aa-misc:image-put-pixel (content-of image)
                                               (symbol->rgb-vector color))))
      (aa:cells-sweep state put-pixel)
      image)))

(defmethod draw-rectangle ((image <image>)
                           x y
                           width height
                           &key
                           (color :black)
                           (fill t)
                           (line-width 1.0)
                           (angle nil)  ;for rotating
                           (round nil)
                           (round-x nil)
                           (round-y nil))
  (if fill
      (let ((paths (paths:make-rectangle-path x y
                                              (+ width x) (+ height y)
                                              :round round
                                              :round-x round-x
                                              :round-y round-y))
            (state (aa:make-state)))
        (if angle (paths:path-rotate paths angle (paths:make-point x y)))
        (let ((put-pixel (aa-misc:image-put-pixel (content-of image)
                                                  (symbol->rgb-vector color))))
          (aa:cells-sweep (vectors::update-state state paths) put-pixel)))
      (progn
        (draw-rectangle image
                        x y
                        width height
                        :color color
                        :angle angle
                        :fill t)
        (draw-rectangle image
                        (+ x line-width)
                        (+ y line-width)
                        (- width (* 2 line-width))
                        (- height (* 2 line-width))
                        :color (background-of image)
                        :angle angle
                        :fill t)
        ))
  image)

(defmethod draw-circle ((image <image>)
                        x y
                        radius
                        &key
                        (color :black))
  (let ((paths (paths:make-circle-path x y radius))
        (state (aa:make-state)))
    (let ((put-pixel (aa-misc:image-put-pixel (content-of image)
                                              (symbol->rgb-vector color))))
      (aa:cells-sweep (vectors::update-state state paths) put-pixel)
      image)))

(defmethod draw-ring ((image <image>)
                      x y
                      radius band
                      &key
                      (ring-color :black)
                      (center-color :white))
  (let ((small-r (- radius band)))
    (if (< small-r 0)
        (error "band must be smaller than radius"))
    (draw-circle image x y radius :color ring-color)
    (draw-circle image x y small-r :color center-color)
    image))

;;Aの縦幅で指定することにする
(defun pixel-font-size->ttf-font-size (pixel-font-size font-loader)
  (let ((paths (paths-ttf:paths-from-string font-loader "A"
                                            :offset (paths-ttf::make-point 0 0)
                                            )))
    (let ((all-knots (reduce #'append
                             (mapcar #'(lambda (x) (coerce (paths::path-knots x) 'cons))
                                     paths))))
      (let ((min-y (apply #'min (mapcar #'cdr all-knots)))
            (max-y (apply #'max (mapcar #'cdr all-knots))))
        (let ((height-in-pixel (- max-y min-y)))
          (abs (/ (float pixel-font-size) (float height-in-pixel))))))))

(defmethod draw-string ((image <image>)
                        str
                        x y             ;左上の点
                        &key
                        (color :black)
                        (font-size 10)) ;in pixel
  (let ((ttf-font-size (pixel-font-size->ttf-font-size
                        font-size
                        (font-loader-of image))))
    (let ((paths (paths-ttf:paths-from-string (font-loader-of image)
                                              str
                                              :offset (paths-ttf::make-point x (+ font-size y))
                                              :scale-x ttf-font-size
                                              :scale-y (- ttf-font-size)))
          (state (aa:make-state)))
      (let ((put-pixel (aa-misc:image-put-pixel (content-of image)
                                                (symbol->rgb-vector color))))
        (aa:cells-sweep (vectors::update-state state paths) put-pixel)
        image))))

(defmethod drawn-string-bounding-box ((font ZPB-TTF::FONT-LOADER)
                                      str
                                      x y
                                      &key
                                      (font-size 0.015))
  (let ((paths (paths-ttf:paths-from-string font
                                            str
                                            :offset (paths-ttf::make-point x y)
                                            :scale-x font-size
                                            :scale-y (- font-size))))
    (let ((all-knots (reduce #'append
                             (mapcar #'(lambda (x) (coerce (paths::path-knots x) 'cons))
                                     paths))))
      (let ((min-x (apply #'min (mapcar #'car all-knots)))
            (max-x (apply #'max (mapcar #'car all-knots)))
            (min-y (apply #'min (mapcar #'cdr all-knots)))
            (max-y (apply #'max (mapcar #'cdr all-knots))))
        (list (cons :min-x min-x)
              (cons :max-x max-x)
              (cons :min-y min-y)
              (cons :max-y max-y))))))

(defmethod drawn-string-size ((font ZPB-TTF::FONT-LOADER)
                              str
                              &key
                              (font-size 0.015))
  (let ((bbox (drawn-string-bounding-box font str 0 0 :font-size font-size)))
      (let ((min-x (cdr (assoc :min-x bbox)))
            (max-x (cdr (assoc :max-x bbox)))
            (min-y (cdr (assoc :min-y bbox)))
            (max-y (cdr (assoc :max-y bbox))))
        (cons (- max-x min-x) (- max-y min-y)))))

;; ???
(defmethod flat-content-of ((image <image>) &optional (to nil))
  (let ((from (content-of image))
        (width (width-of image))
        (height (height-of image)))
    (if (null to)
        (setf to (make-array (* width height 3)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref to (+ (* (+ (* i width) j) 3) 0))
                (aref from i j 2))
          (setf (aref to (+ (* (+ (* i width) j) 3) 1))
                (aref from i j 1))
          (setf (aref to (+ (* (+ (* i width) j) 3) 2))
                (aref from i j 0))))
    to))

(defmethod fill-c-array ((image <image>) to &optional (step 3))
  (with-slots
        (width height)
      image
    (let ((from (content-of image)))
      (dotimes (j width)
        (dotimes (i height)
          (setf (mem-aref to :unsigned-char (+ (* (+ (* i width) j) step) 0))
                (aref from i j 0))
          (setf (mem-aref to :unsigned-char (+ (* (+ (* i width) j) step) 1))
                (aref from i j 1))
          (setf (mem-aref to :unsigned-char (+ (* (+ (* i width) j) step) 2))
                (aref from i j 2))))
      to)))

(defmethod fill-c-array-reverse ((image <image>) to &optional (step 3))
  (with-slots (width height)
      image
    (let ((from (content-of image)))
      (dotimes (i height)
        (dotimes (j width)
          (dotimes (k step)
            (setf (cffi:mem-aref to :unsigned-char (+ (* (+ (* i width) j) step) k))
                  (aref from i j k)))
            ))))
  to)
