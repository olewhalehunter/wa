
;; fluid computation
;; CA material reaction, water/dye deposition

(defpackage #:wa
  (:use #:cl))
(declaim (sb-ext:muffle-conditions cl:warning))
(defun image-setup ()
  (setq quicklisp-reqs '(cl-glut bordeaux-threads))
  (mapcar (lambda (x) (ql:quickload x)) quicklisp-reqs)

  (asdf:load-system 'cl-evdev)
  )

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

(setq color .7)
(defmethod glut:display ((w wa-mask))
  (gl:clear :color-buffer)
  (gl:color color 0 1)
  (gl:with-primitive :polygon
    (gl:vertex 0.25 0.25 0)
    (gl:vertex .75 0.25 0)
    (gl:vertex 0.75 0.75 0)
    (gl:vertex 0.25 0.75 0))
  (gl:flush))

(setq evdev-id "usb-WACOM_CTE-440-U_V4.0-3-mouse"
   evdev-stream '()
   calibrate t)

(setq pressure 0
   abs-x nil
   abs-y nil)

(defun handle-pen (event)
  (with-slots (value type) event
    (let ((axis (cadr type)))
      
      (cond ((equal axis 'ABS_PRESSURE)
	     (setq pressure value))
	    ((equal axis 'ABS_X)
	     (setq abs-x value))
	    ((equal axis 'ABS_Y)
	     (setq abs-y value)))

      (setq color .7)
      (if (> pressure 100)
	  (setq color .5))
      (if (> pressure 200)
	  (setq color 0))
      (glut:post-redisplay)
      )
    )  
  )

(defun handle-scroll (event)
  (with-slots (value) event
    ;; (print value)   
    )
  nil
)

(defun process-wacom-stream ()
  (cl-evdev::with-evdev-device 
      (input "/dev/input/wacom")
    ;; (print input)

    (cond ((typep input 'relative-event)
	   (handle-scroll input))
	  ((typep input 'absolute-event)
	   (handle-pen input))
	  (t nil))    
    ))

(defun debug-raw-evdev ()
  (with-open-file
      (stream  "/dev/input/tablet-graphire4-4x5"
	       :direction :io
	       :element-type '(unsigned-byte 8)
	       :if-does-not-exist :error)
    (loop for x from 1 to 16 do ;; times
	 (format t "~a " (read-byte stream nil)))
    (format t "~%~%")
    ))       

(defun wa ()
  (bordeaux-threads:make-thread
   'process-wacom-stream :name "wacom-stream-process")
  (glut:display-window (make-instance 'wa-mask))
  )

(defun compile-wa-image ()
  (cl-user::save-lisp-and-die "wa"))
(defun compile-wa-exec ()
  (cl-user::save-lisp-and-die "wa" executable t :toplevel 'wa::wa))



