(require :nurikabe)

(defvar *manager* (nurikabe:init-gui :loggingp t :threadingp nil))
(defvar *win* (nurikabe:make-window :width 300
                                    :height 200
                                    :background :white
                                    :name "test window"))

(defun test-line ()
  (nurikabe:draw-line (nurikabe:image-of *win*) 0 0 300 200)
  (nurikabe:flush-window *win* :clear t))
  
(defun test-polygon ()
  (nurikabe:draw-polygon (nurikabe:image-of *win*)
                          '((200 100)
                            (250 150)
                            (50 100)))
  (nurikabe:flush-window *win* :clear t))

(defun test-rectangle (&optional (color :black))
  (nurikabe:draw-rectangle (nurikabe:image-of *win*)
                           150 100
                           100 100
                           :fill t
                           :color color
                           :round 20
                           :angle (nurarihyon:deg2rad 10.0))
  (nurikabe:flush-window *win* :clear t))

(defun test-string (&optional (str "Hello world!"))
  (nurikabe:draw-string (nurikabe:image-of *win*)
                        str
                        0 20
                        :font-size 0.01
                        :color :black)
  (nurikabe:flush-window *win* :clear t))

(defun test-rectangle-and-string ()
  (nurikabe:draw-rectangle (nurikabe:image-of *win*)
                           0 0
                           100 100
                           :fill t
                           :color :red)
  (nurikabe:draw-string (nurikabe:image-of *win*)
                        "Hello World"
                        0 20
                        :font-size 0.01
                        :color :black)
  (nurikabe:flush-window *win* :clear t))

(defun test-circle ()
  (nurikabe:draw-circle (nurikabe:image-of *win*)
                        150 100
                        50 :color :red)
  (nurikabe:flush-window *win* :clear t))

(defun test-ring ()
  (nurikabe:draw-ring (nurikabe:image-of *win*)
                      150 100
                      50 10
                      :ring-color :gray60
                      )
  (nurikabe:flush-window *win* :clear t))

(defun draw-sac-ring (base-angle &optional (center '(150 100)))
  (let ((r 80)
        (band 10))
    (nurikabe:draw-ring (nurikabe:image-of *win*)
                        (car center) (cadr center)
                        r band
                        :ring-color :gray60)
    
    (let ((out-angles (list 10.0
                            140.0
                            260.0))
          (dig-angle 120.0))
      ;; 突起?
      (dolist (a out-angles)
        (let ((angle (nurarihyon:deg2rad (+ base-angle a))))
          (let ((rectangle-center (list (+ (car center) (* (- r (/ band 2))  (cos angle)))
                                        (+ (cadr center) (* (- r (/ band 2)) (sin angle))))))
            (nurikabe:draw-rectangle
             (nurikabe:image-of *win*)
             (car rectangle-center) (cadr rectangle-center)
             band band
             :angle angle
             :fill t
             :color :gray60))))
      ;; 切れ目
      (let ((angle (nurarihyon:deg2rad (+ base-angle dig-angle))))
        (let ((rectangle-center (list (+ (car center) (* (- r band 1) (cos angle)))
                                      (+ (cadr center) (* (- r band 1) (sin angle))))))
          (nurikabe:draw-rectangle
           (nurikabe:image-of *win*)
           (car rectangle-center) (cadr rectangle-center)
           (+ band 2) (+ band 2)
           :angle angle
           :fill t
           :color :white)))
      )))

(defun draw-sac-text (top-text sub-texts base-angle &optional (center '(150 100)))
  (draw-sac-ring base-angle center)
  (let ((text-size 30)
        (font-size 0.01))
    (nurikabe:draw-string (nurikabe:image-of *win*)
                          top-text
                          (- (car center) text-size)
                          (- (cadr center) text-size)
                          :font-size font-size
                          :color :gray30)
    (iterate:iter
      (iterate:for text in sub-texts)
      (iterate:for i from 0)
      ;; text
      (nurikabe:draw-string (nurikabe:image-of *win*)
                            text
                            (- (car center) text-size)
                            (+ (- (cadr center) text-size) (* (1+ i) text-size))
                            
                            :font-size font-size
                            :color :gray30)
      ;; line
      )))

(defun test-sac-animation ()
  (nurikabe:with-x-serialize (*manager*)
    (dotimes (i 100)
      (draw-sac-text "STATE" '("  (PICK OBJ)") i)
      (nurikabe:flush-window *win* :clear t)
      )
    ))

(defun fast-main ()
  (test-line)
  (test-polygon)
  (test-rectangle)
  (test-string)
  (test-circle)
  (test-rectangle-and-string)
  )

(defun main ()
  (test-line)
  (sleep 3)
  (test-polygon)
  (sleep 3)
  (test-rectangle)
  (sleep 3)
  (test-string)
  (sleep 3)
  (test-ring)
  (sleep 3)
  (test-circle)
  (sleep 3)
  (test-rectangle-and-string)
  t)

(main)
