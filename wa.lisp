

;; instream processing thread
;; wacom device values

(defpackage #:wa
  (:use #:cl))
(defun image-setup ()
  (setq quicklisp-reqs '(cl-glut bordeaux-threads))
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
  (gl:ortho 0 1 0 1 -1 1)
  )
(defmethod glut:display ((w wa-mask))
  (gl:clear :color-buffer)
  (gl:color .8 0 1)
  (gl:with-primitive :polygon
    (gl:vertex 0.25 0.25 0)
    (gl:vertex 0.75 0.25 0)
    (gl:vertex 0.75 0.75 0)
    (gl:vertex 0.25 0.75 0))
  (gl:flush))

(setq evdev-id "usb-WACOM_CTE-440-U_V4.0-3-mouse")

(defun process-wacom-stream (in)
  (loop for value = (read-char in nil)
     while value do 
       (format t "~a~%" value)))

(defun handle-wacom ()
  (let ((in (open (concatenate 'string
			       "/dev/input/by-id/" evdev-id) 
		  :if-does-not-exist nil)))
    (when in
      (process-wacom-stream in)
      (close in))))

(defun wa ()
  (bordeaux-threads:make-thread
		'handle-wacom :name 
		"wa-wacom-process")
  (glut:display-window (make-instance 'wa-mask))   
  )

(defun compile-wa-image ()
  (cl-user::save-lisp-and-die "wa"))
(defun compile-wa-exec ()
  (cl-user::save-lisp-and-die "wa" executable t :toplevel 'wa::wa))



