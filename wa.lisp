
(defpackage #:wa
  (:use #:cl))
(declaim (sb-ext:muffle-conditions cl:warning))
(defun image-setup ()
  (setq quicklisp-reqs '(cl-glut bordeaux-threads bit-smasher))
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
  (gl:color .7 0 1)
  (gl:with-primitive :polygon
    (gl:vertex 0.25 0.25 0)
    (gl:vertex 0.75 0.25 0)
    (gl:vertex 0.75 0.75 0)
    (gl:vertex 0.25 0.75 0))
  (gl:flush))

(setq evdev-id "usb-WACOM_CTE-440-U_V4.0-3-mouse"
   evdev-stream '()
   calibrate t)

(defun calibrate-output-stream ()
  (loop for byte = (read-byte evdev-stream nil)
     while byte do    
       (format t "~a~%" (bit-smasher:bits<- byte))
       (format t "~%")
       (finish-output t)
))

(defun process-wacom-stream ()
  (loop for byte = (read-byte evdev-stream nil)
     while byte do     
       (format t "~a~%" (bit-smasher:bits<- byte))
       (format t "wawa~%")))

(defun wa ()
  (let ((in (open (concatenate 'string
			       "/dev/input/by-id/" evdev-id) 
		  :element-type '(unsigned-byte 8)
		  :if-does-not-exist :error)))
    (when in
      (setq evdev-stream in)       
      (format t "!!!!~%")
      (finish-output t)
      (if calibrate
	  (bordeaux-threads:make-thread
	   'calibrate-output-stream :name "calibrate-stream-process")
	  (bordeaux-threads:make-thread
	   'process-wacom-stream :name "wacom-stream-process"))
      (glut:display-window (make-instance 'wa-mask))
      (close in)))
  )

(defun compile-wa-image ()
  (cl-user::save-lisp-and-die "wa"))
(defun compile-wa-exec ()
  (cl-user::save-lisp-and-die "wa" executable t :toplevel 'wa::wa))



