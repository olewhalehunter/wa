;; pixel stroke smooth
;; calligraph xdotool entry
;; large scale + sector update
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

;; (code-char 65)
;; (code-char 97)

(defun get-string-picto ()
  
)

(setq pressure 0.0
   abs-x nil
   abs-y nil)

(defun set-grid (x y value)
  (setf (aref grid x y) value))

(defun handle-pen (event)
  (with-slots (value type) event
    (let ((axis (cadr type)))
      
      (cond ((equal axis 'ABS_PRESSURE) ;; Z?
	     (setq pressure value))
	    ((equal axis 'ABS_X)
	     (setq abs-x value))
	    ((equal axis 'ABS_Y)
	     (setq abs-y value)))
      (print abs-y)
      
      (setq color (/ pressure 700.0))
      (if (> pressure 0)	  
       	  (set-grid 
	   (round (* abs-x .01))
	   (round (* abs-y .013))
	   (list 0 color color)))
      
      (glut:post-redisplay)
      )
    )  
  )

(defun handle-scroll (event)
  (with-slots (value) 
      (print event)   
    )  
  (clear-grid grid-size)
  nil
)

(defun handle-button (event)
  (with-slots (value) event
    (print event)   
    )
)

(defun process-wacom-stream ()
  (cl-evdev::with-evdev-device 
      (input "/dev/input/wacom")
    ;; (print input)

    (cond ((typep input 'relative-event)
	   (handle-scroll input))
	  ((typep input 'absolute-event)
	   (handle-pen input))
	  ((typep input 'keyboard-event)
	   (handle-button input))
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

(defun clear-grid (size)
  (setq grid (make-array (list size size)
		    :initial-element '(0 0 0)))
  (setf (aref grid 10 10) '(1 0 1))
  )
(setq grid-size 100
   grid (clear-grid grid-size))

(defun quad (a b c
	 d e f
	 g h i
	 j k l)
  (gl:with-primitive :polygon
    (gl:vertex a b c)
    (gl:vertex d e f)
    (gl:vertex g h i)
    (gl:vertex j k l))
  )

(defun grid-color (x y)
  (setq rgb (aref grid x y))
  (gl:color (first rgb) 
	    (second rgb)
	    (third rgb))
)
(defmethod glut:display ((w wa-mask))
  (gl:clear :color-buffer)  
  (loop for x from 0 to (- grid-size 1) do   
       (loop for y from 0 to (- grid-size 1) do  
	    (setq j (- grid-size (+ 1 y)))
	    (grid-color x j)
	    (setq factor .01)
	    (quad (* factor x) (* factor y) 0
		  (* factor (+ 1 x)) (* factor y) 0
		  (* factor (+ 1 x)) (* factor (+ 1 y)) 0
		  (* factor x) (* factor (+ 1 y)) 0)
	    ))  
  (gl:flush))

(defun wa ()
  (bordeaux-threads:make-thread
   'process-wacom-stream :name "wacom-stream-process")
  (glut:display-window (make-instance 'wa-mask))
  (cl-user::quit)
  )

(defun compile-wa-image ()
  (cl-user::save-lisp-and-die "wa"))
(defun compile-wa-exec ()
  (cl-user::save-lisp-and-die "wa" executable t :toplevel 'wa::wa))



