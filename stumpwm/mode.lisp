(in-package :stumpwm)

(defun get-battery-bar ()
  (let* ((raw (run-shell-command "acpi | grep 'Battery 0' | awk -F',' '{ print $2 }' | egrep -o '[0-9]+'" t))
	 (inted (parse-integer raw)))
    (values inted (bar inted 25 #\# #\Space))))


(defun get-tags (current-id)
  (mapcar #'(lambda (w)
	      (let ((id (window-number w)))
		`(,id . ,(= current-id id))))
	  (stumpwm::all-windows)))

(defparameter *modeline-tags* '(no pa re ci vo mu xa ze bi so))

(defun display-windows (current)
  (let ((tags (get-tags (window-number current))))
    (stable-sort tags #'(lambda (a b) (< (car a) (car b))))
    (format nil "~{~a~} | ~a"
	    (mapcar #'(lambda (tag) (format nil (if (cdr tag) "(~a)" " ~a ") (nth (car tag) *modeline-tags*)))
		    tags)
	    (window-title current))))

(defparameter *max-chars-per-line* 212) ; Hope I'll find a better way to do that later.

(defun gen-mid-space (left right)
  (make-string (- *max-chars-per-line* (+ (length right) (length left))) :initial-element #\Space))

(defun build-mode-line ()
  (multiple-value-bind (percentage bar) (get-battery-bar)
    (let* ((total-width (stumpwm::head-width (current-head)))
	   (current (current-window))
	   (left (display-windows current))
	   (right (format nil "~a | ~a ~D% | ~a"
			  (string-trim '(#\Return #\Newline #\Linefeed) (run-shell-command "rdate '%a %d %b - %H:%M'" t))
			  bar
        percentage
        (lit-layout *layout*))))
      (format nil "~a~a~a" left (gen-mid-space left right) right))))

(setf *mode-line-background-color* "#2e3440")
(setf *mode-line-foreground-color* "#eceff4")
(setf *mode-line-position* :bottom)
(setf *mode-line-timeout* 5)
(enable-mode-line (current-screen) (current-head) t)
(setf *screen-mode-line-format* (list '(:eval (build-mode-line))))
