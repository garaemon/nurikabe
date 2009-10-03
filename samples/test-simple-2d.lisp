(require :nurikabe)

(defvar *manager* (nurikabe:init-gui))
(defvar *win* (nurikabe:make-window :width 300
                                    :height 200
                                    :background :white
                                    :name "test window"))

(defun test-line ()
  (nurikabe:draw-line (nurikabe:image-of *win*) 0 0 300 200)
  (nurikabe:flush-window *win* :clear t)
  (nurikabe:flush *manager*))
  
(defun test-polygon ()
  (nurikabe:draw-polygon (nurikabe:image-of *win*)
                          '((200 100)
                            (250 150)
                            (50 100)))
  (nurikabe:flush-window *win* :clear t)
  (nurikabe:flush *manager*))

(defun test-rectangle (&optional (color :black))
  (nurikabe:draw-rectangle (nurikabe:image-of *win*)
                           20 20
                           100 100
                           :fill t
                           :color color
                           :round 20
                           )
  (nurikabe:flush-window *win* :clear t)
  (nurikabe:flush *manager*))

(defun test-string (&optional (str "Hello world!"))
  (nurikabe:draw-string (nurikabe:image-of *win*)
                        str
                        0 20
                        :font-size 0.01
                        :color :black)
  (nurikabe:flush-window *win* :clear t)
  (nurikabe:flush *manager*))

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
  (nurikabe:flush-window *win* :clear t)
  (nurikabe:flush *manager*))

(defun test-circle ()
  (nurikabe:draw-circle (nurikabe:image-of *win*)
                        150 100
                        50 :color :red)
  (nurikabe:flush-window *win* :clear t)
  (nurikabe:flush *manager*))

(defun main ()
  (test-line)
  (sleep 3)
  (test-polygon)
  (sleep 3)
  (test-rectangle)
  (sleep 3)
  (test-string)
  (sleep 3)
  (test-circle)
  (sleep 3)
  (test-rectangle-and-string)
  t)

(main)
