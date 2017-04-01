

;; evdev read
;; stream processing

(defpackage #:wa
  (:use #:cl))
(defun image-setup ()
  (setq quicklisp-reqs '(cl-glut))
  (mapcar (lambda (x) (ql:quickload x)) quicklisp-reqs))


(defclass wa-mask
    (glut:window) ()
  (:default-initargs :pos-x 100 :pos-y 100
		     :width 250 :height 250
                     :mode '(:single :rgb) :title ""))
(defmethod glut:display-window :before ((w wa-mask))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))
(defmethod glut:display ((w wa-mask))
  (gl:clear :color-buffer)
  (gl:color .8 0 1)
  (gl:with-primitive :polygon
    (gl:vertex 0.25 0.25 0)
    (gl:vertex 0.75 0.25 0)
    (gl:vertex 0.75 0.75 0)
    (gl:vertex 0.25 0.75 0))
  (gl:flush))

(defun wa ()
  (glut:display-window (make-instance 'wa-mask)))





