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
