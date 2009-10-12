(require :nurikabe)

(defvar *manager* (nurikabe:init-gui))
(defvar *win* (nurikabe:make-window :width 300
                                    :height 200
                                    :background :white))

(defun test-click-button-vertical ()
  (nurikabe:delete-widgets *win* :flush nil)
  (let ((vbox (nurikabe:make-packing-box *win* :vertical)))
    (let ((buttons (iterate:iter
                    (iterate:for i from 0 to 5)
                    (iterate:collect (nurikabe:make-click-button
                                      *win*
                                      (format nil "~A!" i)
                                      :x 10 :y 10
                                      :press-callback
                                      #'(lambda () (format t "clicked~%")))))))
      (apply #'nurikabe:line-up vbox buttons)))
  (nurikabe:flush *manager*))

(defun test-click-button-horizontal ()
  (nurikabe:delete-widgets *win* :flush nil)
  (let ((vbox (nurikabe:make-packing-box *win* :horizontal)))
    (let ((buttons (iterate:iter
                     (iterate:for i from 0 to 1)
                    (iterate:collect (nurikabe:make-click-button
                                      *win*
                                      (format nil "~A!" i)
                                      :x 10 :y 10
                                      :press-callback
                                      #'(lambda () (format t "clicked~%")))))))
      (apply #'nurikabe:line-up vbox buttons)))
  (nurikabe:map-widgets *win*)
  (nurikabe:flush *manager*))

(defun test-packing-box ()
  (nurikabe:delete-widgets *win* :flush nil)
  (let ((vbox (nurikabe:make-packing-box *win* :vertical)))
    (let ((toggle-widget1 (nurikabe:make-toggle-button
                           *win*
                           "toggle button1"
                           :x 10 :y 40))
          (toggle-widget2 (nurikabe:make-toggle-button
                           *win*
                           "toggle button2"
                           :x 10 :y 10)))
      (nurikabe:line-up vbox toggle-widget1 toggle-widget2)))
  (nurikabe:map-widgets *win*)
  (nurikabe:flush *manager*))

(defun main ()
  (test-click-button-vertical)
  (sleep 1)
  (test-click-button-horizontal)
  (sleep 1)
  (test-packing-box)
  (sleep 1)
  )

(dotimes (i 10)
  (main))
;;(main)

