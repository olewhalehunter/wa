(defpackage :wa
  (:use #:cl))
(in-package :wa)
(load "utils.lisp")
(load "cl-event-handler/cl-event-handler.asd")
(load "cl-evdev/cl-evdev.asd")
(mapcar
 #'ql:quickload
 '(cl-glut 
   bordeaux-threads
   cl-event-handler
   cl-evdev))

(defparameter evdev-device-file "/dev/input/event16")

(defun set-device (device-file)
  (setq evdev-device-file device-file))

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
   grid-size 300 grid (clear-grid grid-size))

(defun handle-pen (event)
  (with-slots (cl-evdev::value cl-evdev::type) event
    (let ((axis (cadr type)))
      (print `(AXIS IS ,axis))
      (cond ((equal axis 'cl-evdev::ABS_PRESSURE) (setq pressure cl-evdev::value))
	    ((equal axis 'cl-evdev::ABS_X) (setq abs-x cl-evdev::value))
	    ((equal axis 'cl-evdev::ABS_Y) (setq abs-y cl-evdev::value)))      
      (setq color (/ pressure 700.0))
      
      (print `(ABS_X ,abs-x 'ABS_Y ,abs-y))
      (print `(ABS_PRESSURE ,pressure))
      (if (> pressure 0)	  
       	  (set-grid  (round (* abs-x .020))	   
		     (round (* abs-y .040))
		     (list 0 0 color)))
      (glut:post-redisplay))))

(defun handle-scroll (event)
  (with-slots (cl-evdev::value) event
      (if (= cl-evdev::value 1)
	  (setq tool-size (+ 1 tool-size))
	  (setq tool-size (- 1 tool-size)))))

(defun handle-button (event)
  (with-slots (cl-evdev::name) event
    (if (equal cl-evdev::name 'BTNBACK)
	(clear-grid grid-size))
    (with-slots (spin) mask
      (setf spin (mod (+ spin 30) 360)))))

(defun process-wacom-stream ()
  (cl-evdev::with-evdev-device 
      (in evdev-device-file)
    (print `(type of ,(Type-of in)))
    (cond ((typep in 'cl-evdev::RELATIVE-EVENT)
	   (handle-scroll in))
	  ((typep in 'cl-evdev::ABSOLUTE-EVENT)
	   (handle-pen in))
	  ((typep in 'cl-evdev::KEYBOARD-EVENT)
	   (handle-button in))
	  (t nil))))

(defclass wa-mask (glut:window)
  ((spin :initform 0))
  (:default-initargs :pos-x 0 :pos-y 0
		     :width 2250 :height 2250
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
	       factor .0018)
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
