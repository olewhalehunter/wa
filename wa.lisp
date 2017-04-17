(defpackage #:wa (:use #:cl))
(defun image-setup ()
  (mapcar (lambda (x) (ql:quickload x)) '(cl-glut 
				     bordeaux-threads))
  (push "/path/to/cl-evdev/"
	asdf:*central-registry*)
  (asdf:load-system 'cl-evdev))
(defun compile-wa-image ()
  (cl-user::save-lisp-and-die "wa"))
(defun compile-wa-exec ()
  (cl-user::save-lisp-and-die "wa-demo" :executable t :toplevel 'wa))

(defun set-grid (x y value)
  (setf (aref grid x y) value)
  (if (and (> tool-size 1) (< x grid-size) (< y grid-size))
      (loop for i from x to (- 1 (+ x tool-size))
	 do (loop for j from y to (- 1 (+ y tool-size))
	       do (if (and (< i grid-size) (< j grid-size))
		      (setf (aref grid i j) value))))))
(defun clear-grid (size)
  (setq grid (make-array (list size size)
		      :initial-element '(0 0 0))
     blue '(0 0 1) )
  (set-grid 0 0 blue)
  (set-grid 0 (- grid-size 1) blue)
  (set-grid (- grid-size 1) (- grid-size 1) blue)
  (set-grid (- grid-size 1) 0 blue)
  grid)
(setq pressure 0.0 abs-x 0 abs-y 0
   tool-size 1 mask nil
   grid-size 200 grid (clear-grid grid-size))

(defun handle-pen (event)
  (with-slots (value type) event
    (let ((axis (cadr type)))
      (cond ((equal axis 'ABS_PRESSURE) (setq pressure value))
	    ((equal axis 'ABS_X) (setq abs-x value))
	    ((equal axis 'ABS_Y) (setq abs-y value)))      
      (setq color (/ pressure 700.0))      
      (if (> pressure 0)	  
       	  (set-grid  (round (* abs-x .020))	   
		     (round (* abs-y .023))
		     (list 0 0 color)))
      (glut:post-redisplay))))
(defun handle-scroll (event)
  (with-slots (value) event
      (if (= value 1)
	  (setq tool-size (+ 1 tool-size))
	(setq tool-size (- 1 tool-size)))))
(defun handle-button (event)
  (with-slots (name) event
    (if (equal name 'BTNBACK)
	(clear-grid grid-size))
    (with-slots (spin) mask
      (setf spin (mod (+ spin 30) 360)))))

(defun process-wacom-stream ()
  (cl-evdev::with-evdev-device 
      (in "/dev/input/wacom")
    (cond ((typep in 'RELATIVE-EVENT)
	   (handle-scroll in))
	  ((typep in 'ABSOLUTE-EVENT)
	   (handle-pen in))
	  ((typep in 'KEYBOARD-EVENT)
	   (handle-button in))
	  (t nil))))

(defclass wa-mask (glut:window)
  ((spin :initform 0))
  (:default-initargs :pos-x 200 :pos-y 200
		     :width 750 :height 750
                     :mode '(:single :rgb) :title "wa"))

(defmethod glut:display-window :before ((w wa-mask))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))

(defun quad (a b c  d e f
	 g h i  j k l)
  (gl:with-primitive :polygon
    (gl:vertex a b c)
    (gl:vertex d e f)
    (gl:vertex g h i)
    (gl:vertex j k l)))

(defun grid-color (x y)
  (setq rgb (aref grid x y))
  (gl:color (first rgb)  (second rgb) (third rgb))
  rgb)

(defmethod glut:display ((w wa-mask))
  (gl:clear :color-buffer)  
  (gl:disable :lighting)
  (loop for x from 0 to (- grid-size 1) do   
       (loop for y from 0 to (- grid-size 1) do  
	    (setq j (- grid-size (+ 1 y))
	       factor .0048)
	    (if (not (eq (grid-color x j) '(0 0 0)))		
		(quad (* factor x) (* factor y) 0
		      (* factor (+ 1 x)) (* factor y) 0
		      (* factor (+ 1 x)) (* factor (+ 1 y)) 0
		      (* factor x) (* factor (+ 1 y)) 0))
	    ))
  (gl:color 0 0 1)
  (gl:flush))

(defun gl-window ()
  (setq mask (make-instance 'wa-mask))
  (glut:display-window mask))
(defun wa ()
  (bordeaux-threads:make-thread
   'process-wacom-stream :name "wacom-stream-process")
  (bordeaux-threads:make-thread
   'gl-window :name "gl window"))
