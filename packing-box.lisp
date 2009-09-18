;;====================================================================
;; packing-box.lisp
;;
;; "packing box" is a utility for setting widgets in window in
;; the neet order.
;;
;; You can use vertical/horizontal packing box.
;;
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================

(in-package #:nurikabe)

(defmethod make-packing-box ((win <window>) type)
  (case type
    (:vertical
     (make-instance '<vertical-packing-box>
                    :window win
                    :root-packing-box nil))
    (:horizontal
     (make-instance '<horizontal-packing-box>
                    :window win
                    :root-packing-box nil))
    (t
     (error "unkown type ~A -- MAKE-PACKING-BOX --" type))))

(defmethod make-packing-box ((pbox <packing-box>) type)
  (case type
    (:vertical
     (make-instance '<vertical-packing-box>
                    :root-packing-box pbox
                    :window nil))
    (:horizontal
     (make-instance '<horizontal-packing-box>
                    :root-packing-box pbox
                    :window nil))
    (t
     (error "unkown type ~A -- MAKE-PACKING-BOX --" type))
    ))

(defmethod x-position ((packing-box <packing-box>) index)
  (if (root-packing-box-of packing-box)
      (x-position/packing-box packing-box index)
      (x-position/win packing-box index)))

(defmethod y-position ((packing-box <packing-box>) index)
  (if (root-packing-box-of packing-box)
      (y-position/packing-box packing-box index)
      (y-position/win packing-box index)))

(defmethod x-position/win ((packing-box <vertical-packing-box>) index)
  (let ((widget (elt (widgets-of packing-box) index)))
    (let ((widget-width (width-of widget))
          (window-width (width-of (window-of packing-box))))
      ;; <------window-width----->
      ;; <---> widget-width <---->
      (/ (- window-width widget-width) 2))))

(defmethod y-position/win ((packing-box <vertical-packing-box>) index)
                       
  (let ((widget (elt (widgets-of packing-box) index)))
    (let ((window-height (height-of (window-of packing-box))))
      ;; <------------window-height-------------->
      ;; <---> 1 <----> 2 ... i ... <----> n <--->
      (let ((step (/ (- window-height
                        (apply #'+ (mapcar #'height-of (widgets-of packing-box))))
                     (1+ (length (widgets-of packing-box))))))
        (+ step (iterate:iter (iterate:for i from 0 to (1- index))
                              (iterate:for w in (widgets-of packing-box))
                              (iterate:sum (+ step (height-of w)))))))))

(defmethod y-position/win ((packing-box <horizontal-packing-box>) index)
  (let ((widget (elt (widgets-of packing-box) index)))
    (let ((widget-height (height-of widget))
          (window-height (height-of (window-of packing-box))))
      ;; <------window-height----->
      ;; <---> widget-height <---->
      (/ (- window-height widget-height) 2))))

(defmethod x-position/win ((packing-box <horizontal-packing-box>) index)
  (let ((widget (elt (widgets-of packing-box) index)))
    (let ((window-width (width-of (window-of packing-box))))
      ;; <------------window-width-------------->
      ;; <---> 1 <----> 2 ... i ... <----> n <--->
      (let ((step (/ (- window-width
                        (apply #'+ (mapcar #'width-of (widgets-of packing-box))))
                     (1+ (length (widgets-of packing-box))))))
        (+ step (iterate:iter (iterate:for i from 0 to (1- index))
                              (iterate:for w in (widgets-of packing-box))
                              (iterate:sum (+ step (width-of w)))))))))


(defmethod line-up ((packing-box <packing-box>) &rest widgets)
  (let ((target-window (window-of packing-box)))
    (setf (widgets-of packing-box) widgets)
    (iterate:iter
     (iterate:for i from 0 to (1- (length widgets)))
     (iterate:for w in widgets)
     (let ((x-position (x-position packing-box i))
           (y-position (y-position packing-box i)))
       (move w (round x-position) (round y-position))))
    widgets))

