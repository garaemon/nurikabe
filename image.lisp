;;================================================
;; image.lisp
;;
;; this code provides <image> class and methods.
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (debug 3)
		   (safety 3)))

(in-package #:nurikabe)

(defvar *font-paths*
  #+:darwin
  '(#p"/Library/Fonts/"
    #p"/System/Library/Fonts/"
    #p"/usr/X11/lib/X11/fonts/TTF/")
  #+:linux
  '(#p"/usr/share/fonts/truetype/ttf-bitstream-vera/"
    #p"/usr/share/fonts/truetype/ttf-dejavu/")
  "nurikabe search fonts in *font-paths*")

(defvar *default-font*
  #+:darwin
  "VeraMono.ttf"
  #+:linux
  "DejaVuSansMono.ttf"
  "default-font in nurikabe.
   nurikabe only supports the fixed-width fonts")

;; for printing
(defmethod print-object ((image <image>) stream)
  "print width and height when <image> printing."
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
                   (font *default-font*))
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
                 (aa-misc:make-image
                  width height (symbol->rgb-vector background)))
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
                      (color :black)
                      (width 2))
  "Draw a line from (from-x from-y) to (to-x to-y) in a image.

   You can set the color of line by :color keyword."
  ;; using draw-rectangle and :angle here
  (let ((cx (/ (+ from-x to-x) 2))
        (cy (/ (+ from-y to-y) 2)))
    (let ((distance (nh:distance (nh:double-vector from-x from-y 0)
                                 (nh:double-vector to-x to-y 0)))
          (theta (atan (- to-y cy) (- to-x cx))))
      (let ((fx (round (- cx (/ distance 2))))
            (fy (round (- cy (/ width 2)))))
        (draw-rectangle image fx fy
                        distance width
                        :color color :angle theta)))
    image))

(defmethod draw-triangle ((image <image>) a b c
                          &key
                          (color :black)
                          (angle nil)
                          (rotate-center nil)
                          (fill t))
  ;; only supports :fill t
  (let ((path (paths:create-path :closed-polyline))
        (state (aa:make-state)))
    (paths:path-extend path (paths:make-straight-line)
                       (paths:make-point (car a) (cadr a)))
    (paths:path-extend path (paths:make-straight-line)
                       (paths:make-point (car b) (cadr b)))
    (paths:path-extend path (paths:make-straight-line)
                       (paths:make-point (car c) (cadr c)))
    (if angle (paths:path-rotate
                   path angle
                   (if rotate-center
                       (paths:make-point (car rotate-center)
                                         (cadr rotate-center))
                       (paths:make-point
                        (/ (apply #'+ (mapcar #'car (list a b c))) 3)
                        (/ (apply #'+ (mapcar #'cadr (list a b c))) 3)))))
    (let ((put-pixel (aa-misc:image-put-pixel (content-of image)
                                              (symbol->rgb-vector color))))
      (aa:cells-sweep (vectors::update-state state path) put-pixel)))
  image)

   
(defmethod draw-polygon ((image <image>) points &key (color :black))
  "draw a polygon.
   The points must be a CCW.

   You can set the color of line by :color keyword."
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
                           (round-x round)
                           (round-y round))
  (if fill
      (let ((paths (paths:make-rectangle-path x y
                                              (+ width x) (+ height y)
                                              :round round
                                              :round-x round-x
                                              :round-y round-y))
            (state (aa:make-state)))
        (if angle (paths:path-rotate
                   paths angle
                   (paths:make-point (+ x (/ width 2))
                                     (+ y (/ height 2)))))
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
                        :fill t)))
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
  "Convert from pixel-font-size to ttf-font-size.
   It use #\A as a representation character."
  (let ((paths (paths-ttf:paths-from-string
                font-loader "AA"
                :offset (paths-ttf::make-point 0 0))))
    (let ((all-knots
           (reduce #'append
                   (mapcar #'(lambda (x) (coerce (paths::path-knots x) 'cons))
                           paths))))
      (let ((min-x (apply #'min (mapcar #'car all-knots)))
            (max-x (apply #'max (mapcar #'car all-knots))))
        (let ((width-in-pixel (/ (- max-x min-x) 2.0)))
          (abs (/ (float pixel-font-size) (float width-in-pixel))))))))

(defmethod %draw-string ((image <image>)
                         str
                         x y             ;左上の点
                         &key
                         (color :black)
                         (font-size 10)
                         (ttf-font-size 0.1)) ;in pixel
  (let ((paths (paths-ttf:paths-from-string
                (font-loader-of image)
                str
                :offset (paths-ttf::make-point x (+ font-size y))
                :scale-x ttf-font-size
                :scale-y (- ttf-font-size)))
        (state (aa:make-state)))
    (let ((put-pixel (aa-misc:image-put-pixel (content-of image)
                                              (symbol->rgb-vector color))))
      (aa:cells-sweep (vectors::update-state state paths) put-pixel)
      image)))

(defmethod characters-in-line ((image <image>) str font-size
                               &key (x 0) (y 0) (-x 0))
  (declare (ignore y))
  (let ((ttf-font-size (pixel-font-size->ttf-font-size
                        font-size (font-loader-of image))))
    (let* ((image-width (width-of image))
           (rest-width (- (- image-width x) -x))
           (character-width (ttf-font-size->pixel-font-width
                              ttf-font-size (font-loader-of image)))
           (character-height (ttf-font-size->pixel-font-height
                              ttf-font-size (font-loader-of image)))
           (string-length (* character-width (length str)))
           (line-num (ceiling (/ string-length rest-width)))
           (chars-per-line (floor (/ rest-width character-width))))
      (values line-num chars-per-line character-height))))

(defmethod draw-string ((image <image>)
                        str
                        x y             ;左上の点
                        &key
                        (color :black)
                        (-x 0)
                        (auto-newline t)
                        (font-size 10)) ;in pixel
  (let ((ttf-font-size (pixel-font-size->ttf-font-size
                        font-size
                        (font-loader-of image))))
    (if auto-newline
        (multiple-value-bind (line-num chars-per-line char-height)
            (characters-in-line image str font-size :x x :y y :-x -x)
          (dotimes (i line-num)
            (let* ((yy (+ y (* i char-height)))
                   (start-index (* i chars-per-line))
                   (end-index (+ start-index chars-per-line))
                   (%str (subseq str
                                 start-index
                                 (if (> end-index (length str))
                                     (length str)
                                     end-index))))
              (%draw-string image %str x yy
                            :color color
                            :font-size font-size
                            :ttf-font-size ttf-font-size))))
        (%draw-string image str x y
                      :color color
                      :font-size font-size
                      :ttf-font-size ttf-font-size))))

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
    (let ((all-knots
           (reduce #'append
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
  (let ((bbox (drawn-string-bounding-box font str
                                         0 0 ;dummy
                                         :font-size font-size)))
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
  "copy content of image to flat c array."
  (with-slots (width height) image
    (let ((from (content-of image))
          ;; rgb specification
          ;; from ... (r g b)
          ;; to  ...  (b g r)
          (from-green-index 1)
          (to-green-index 1)
          (from-red-index 0)
          (to-red-index 2)
          (from-blue-index 2)
          (to-blue-index 0))
      (dotimes (j width)
        (dotimes (i height)
          (let ((target-pixel-index (* (+ (* i width) j) step)))
            ;; red
            (setf (mem-aref to :unsigned-char
                            (+ target-pixel-index to-red-index))
                  (aref from i j from-red-index))
            ;; green
            (setf (mem-aref to :unsigned-char
                            (+ target-pixel-index to-green-index))
                  (aref from i j from-green-index))
            ;; blue
            (setf (mem-aref to :unsigned-char
                            (+ target-pixel-index to-blue-index))
                  (aref from i j from-blue-index)))))
      to)))

(defmethod fill-c-array-reverse ((image <image>) to &optional (step 3))
  (with-slots (width height) image
    (let ((from (content-of image)))
      (dotimes (i height)
        (dotimes (j width)
          (dotimes (k step)
            (setf (cffi:mem-aref to :unsigned-char
                                 (+ (* (+ (* i width) j) step) k))
                  (aref from i j k)))))))
  to)
