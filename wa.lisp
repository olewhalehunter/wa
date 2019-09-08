(defpackage :wa
  (:use #:cl))

(in-package :wa)

(defun load-dependencies ()
  (load "utils.lisp")  
  (load "cl-event-handler/cl-event-handler.asd")
  (load "cl-evdev/cl-evdev.asd")
  (mapcar
   #'ql:quickload
   '(local-time
     cl-glut 
     bordeaux-threads
     cl-event-handler
     cl-evdev
     ))
  (load "recognition.lisp"))
(load-dependencies) ;; (ql:quickload :local-time)

(defun compile-wa-image ()
  (cl-user::save-lisp-and-die "wa"))

(defun compile-wa-exec ()
  (cl-user::save-lisp-and-die
   "wa-demo" :executable t :toplevel 'wa))



(defun pen-draw (x y value)
  "on the pixel coordinates X Y draw VALUE"
  ;;(print value)
  (setf (aref grid x y) value)
  (if (and (> tool-size 1) (< x grid-size) (< y grid-size))
      (loop for i from x below (- 1 (+ x tool-size))
	 do (loop for j from y to (- 1 (+ y tool-size))
	       do (if (and (< i grid-size) (< j grid-size))
		      (setf (aref grid i j) value))))))


(defun set-grid (x y value)
  "set the canvas state at coordinates X Y to VALUE"
  (setf (aref grid x y) value)
  (if (and (> tool-size 1) (< x grid-size) (< y grid-size))
      (loop for i from x to (- 1 (+ x tool-size))
	 do (loop for j from y below (- 1 (+ y tool-size))
	       do (if (and (< i grid-size) (< j grid-size))
		      (setf (aref grid i j) value))))))

(defun clear-color (value)
  (loop for x from 0 upto grid-size do
       (loop for y from 0 upto grid-size do
	    (set-grid x y value)))
  (glut:post-redisplay))

(defun sample-script ()
  (clear-color svart)
  (set-grid 1 1 svart)
  (set-grid 0 (- grid-size 1) hvit)
  (set-grid (- grid-size 1) 0 purple))

;; (sample-script)


(defparameter blue '(0 0 1))
(defparameter red '(1 0 0))
(defparameter purple '(1 0 1))
(defparameter hvit '(1 1 1))
(defparameter svart '(0 0 0))

(defun new-grid (size)
  "new grid state"
  (setq grid (make-array (list (* size size) (* size size))
		      :initial-element '(0 0 0))
     blue '(0 0 1) )
  (set-grid 0 0 blue)
  (set-grid 0 (- grid-size 1) blue)
  (set-grid (- grid-size 1) (- grid-size 1) blue)
  (set-grid (- grid-size 1) 0 blue)
  grid)

(defparameter evdev-devices
  '((keyboard "/dev/input/event15")
    (wacom "/dev/input/event16")
    ))

(defparameter evdev-device-file
  (second (assoc 'wacom evdev-devices :test #'string=)))

(setq pressure 0.0
      abs-x 0
      abs-y 0
      tool-size 10
      mask nil
      grid-size 100
      grid (new-grid grid-size))

(defun set-device (device-file)
  "configure the input device file location"
  (setq evdev-device-file device-file))

(defun handle-pen (event)
  "pen event handler for events like ABS_PRESSURE, ABS_X..."
  (with-slots (cl-evdev::value cl-evdev::type) event
    (let ((axis (cadr type)))
      (cond ((equal axis 'cl-evdev::ABS_PRESSURE) (setq pressure cl-evdev::value))
	    ((equal axis 'cl-evdev::ABS_X) (setq abs-x cl-evdev::value))
	    ((equal axis 'cl-evdev::ABS_Y) (setq abs-y cl-evdev::value)))      
      ;;(print (list abs-x abs-y)) 
      (setq pressure (coerce pressure 'float))
      (if (> pressure 5)
	  (loop for r from 0 upto tool-size do
	       (pen-draw (round (/ (+ r abs-x) 58.0))
			 (round (/ (+ r abs-y) 88.0))
			 (pen-draw-color)
			 )) ))))

(setq tool-size 40)
(defparameter pen-hue '(0.0 0.0 0.3))
(defparameter pressure-const 311.0)


(defun pen-draw-color () `(0 0 0))
(defun pen-draw-color ()
  `(,(+ 0 (first pen-hue))
    ,(+ (second pen-hue) (/ pressure pressure-const))
   ,(+ 0 (third pen-hue))))

(defun handle-scroll (event)
  "scroll event handler"
  (with-slots (cl-evdev::value) event
      (if (= cl-evdev::value 1)
	  (setq tool-size (+ 1 tool-size))
	  (setq tool-size (- 1 tool-size)))))

(defun handle-button (event)
  "button event handler"
  (with-slots (cl-evdev::name) event
    (if (equal cl-evdev::name 'BTNBACK)
	(clear-color svart))
    (with-slots (spin) mask
      (setf spin (mod (+ spin 30) 360)))))

(defun process-wacom-stream ()
  "Wacom RELATIVE ABSOLUTE or KEYBOARD event process"
  (cl-evdev::with-evdev-device 
      (in evdev-device-file)
    ;;(print `(type of ,(Type-of in)))
    (cond ((typep in 'cl-evdev::RELATIVE-EVENT)
	   (handle-scroll in))
	  ((typep in 'cl-evdev::ABSOLUTE-EVENT)
	   (handle-pen in))
	  ((typep in 'cl-evdev::KEYBOARD-EVENT)
	   (handle-button in))
	  (t nil))
    (glut:post-redisplay)))

(defclass wa-mask (glut:window)
  ((spin :initform 0))
  (:default-initargs :pos-x 0 :pos-y 0
		     :width 2250 :height 2250
                     :mode '(:single :rgb) :title "wa"))

(defmethod glut:display-window :before ((w wa-mask))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -3 1))

(defun quad (a b c  d e f
	     g h i  j k l)
  "create quad by loading vertices into buffers"
  (gl:with-primitive :polygon
    (gl:vertex a b c)
    (gl:vertex d e f)
    (gl:vertex g h i)
    (gl:vertex j k l)))

(defun grid-color (x y)
  "set the GL and grid drawing color"
  (setq rgb (aref grid x y))
  (gl:color (first rgb)  (second rgb) (third rgb))
  rgb)

(defun closnuf (x y)
  (< (abs (- x y)) 5))


(defmethod glut:display ((w wa-mask))
  "OpenGL display draw loop, quad drawing"
  (gl:clear :color-buffer)  
  (gl:disable :lighting)
  (loop for x from 0 to (- grid-size 1) do   
       (loop for y from 0 to (- grid-size 1) do  
	    (setq j (- grid-size (+ 1 y))
		  factor .010) ;; 0018
	    (setq px (floor (/ abs-x 58.0))
		  py (floor (/ abs-y 88.0)))
;;	    (gl:color (first rgb)  (second rgb) (third rgb))	    
	    (if (not (eq (grid-color x j) '(0 0 0)))		
		(quad (* factor x) (* factor y) 0
		      (* factor (+ 1 x)) (* factor y) 0
		      (* factor (+ 1 x)) (* factor (+ 1 y)) 0
		      (* factor x) (* factor (+ 1 y)) 0))
	    (setq rgb '(1 1 1))
	    (if (closnuf y py)
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
  "Wa main threads init"
  (bordeaux-threads:make-thread
   'process-wacom-stream :name "wacom-stream-process")
  (bordeaux-threads:make-thread
   'gl-window :name "gl window"))
;; (wa::wa)
