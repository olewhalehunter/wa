
;; no device case

(defpackage #:wa
  (:use #:cl))
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
   calibrate nil)

(defun calibrate-output-stream ()
  (format t "~a~%" (bit-smasher:bits<- byte)))

(defun process-wacom-stream ()
  (loop for byte = (read-byte wacom-stream nil)
     while byte do        
       (format t "~%")))

(defun wa ()
  (let ((in (open (concatenate 'string
			       "/dev/input/by-id/" evdev-id) 
		  :element-type '(unsigned-byte 8)
		  :if-does-not-exist :error)))
    (when in
      (setq wacom-stream in)
      (glut:display-window (make-instance 'wa-mask)) 

      (if calibrate
	  (bordeaux-threads:make-thread
	   'process-wacom-stream :name
	   "wacom-stream-process")
	  (bordeaux-threads:make-thread
	   'diagnose-wacom-stream :name
	   "wacom-stream-process"))
      (close in)))
  )

(defun compile-wa-image ()
  (cl-user::save-lisp-and-die "wa"))
(defun compile-wa-exec ()
  (cl-user::save-lisp-and-die "wa" executable t :toplevel 'wa::wa))



