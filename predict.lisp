;; Define the function that takes input from json, and predict whether the current robot location is out of the expected route
;; Yang Xu
;; 9/29/2014

(defparameter *robot-current-pos* nil)
(defparameter *robot-previous-pos* nil)
(defparameter *robot-goal-pos* nil)
(defparameter *robot-goal-history* nil)

(defparameter *output* nil)
(defparameter *pos-file* "../glassfish/4.1/libexec/glassfish/domains/domain1/docroot/era_actr_pose.txt")
(defparameter *goal-file* "../glassfish/4.1/libexec/glassfish/domains/domain1/docroot/era_actr_goal.txt")
(defparameter *map-file* "../glassfish/4.1/libexec/glassfish/domains/domain1/docroot/era_actr.gif")

(defparameter *local-height* 2080)
(defparameter *local-width* 224)
(defparameter *actr-height* nil)
(defparameter *actr-width* nil)

;; load packages
(ql:quickload "split-sequence")
(ql:quickload "trivial-shell")

;; the function that reads robot current/goal location from local files
(defun read-file (filename)
	(let ((in (open filename)))
		(setq str (format nil "~a" (read-line in)))
		(close in)
		(setq str (cl-ppcre::scan-to-strings "\\(.*\\)" str)) ;; substring within the parenthesis
		(setq str (subseq str 1 (- (length str) 1)))
		(setq str-li (split-sequence:split-sequence #\, str)) ;; split the string into list
		(setq posx (read-from-string (car str-li)))
		(setq posy (read-from-string (car (cdr str-li))))
		(cons posx posy)))

;; the function that reads pose/goal location from tcp port
(defun read-tcp (target)
	(let ((cmd (concatenate 'string "echo \"" target "\" | nc localhost 7721")))
		(setq return-val (trivial-shell:shell-command cmd))
		(unless (or (string= return-val "") (string= (subseq return-val 0 8) "ERA:null"))
			(setq val (cl-ppcre::scan-to-strings "\\(.*\\)" return-val)) ;; substring within the parenthesis
			(setq val (subseq val 1 (- (length val) 1)))
			(setq val-li (split-sequence:split-sequence #\, val)) ;; split the string into list
			(setq posx (read-from-string (car val-li)))
			(setq posy (read-from-string (car (cdr val-li))))
			(cons posx posy))))

;; the function that compares current pos with the planned route
(defun decide (pos)
	(when (> (length *visual-trace-path*) 1)
		(let ((p0 (car (last *visual-trace-path*)))
			(p1 (nth (- (length *visual-trace-path*) 2) *visual-trace-path*)))
		(setq vo (vectors::p- p1 p0))
		(if (/= (vectors::point-norm vo) 0)
			(setq vo (vectors::p* vo (/ 1 (vectors::point-norm vo)))))
		(setq vn (vectors::p- pos p0))
		(if (/= (vectors::point-norm vn) 0)
			(setq vn (vectors::p* vn (/ 1 (vectors::point-norm vn)))))
		(setq product (+ (* (car vo) (car vn)) (* (cdr vo) (cdr vn))))
		(when (< product 0) t))))

;; the function that triggers explanation
(defun trigger ()
	(trivial-shell:shell-command (concatenate 'string "echo \"ex\" | nc localhost 7721"))
	(print "explanation triggered!"))

;; the function that transforms the coordinates to fit the t3v1.png
(defun trans-coor (pos)
	(let ((pos-x (car pos))
		(pos-y (cdr pos)))
	(setq pos-y (- *actr-height* pos-y))
	(cons pos-x pos-y)))

;; the function that sets parameters
(defun set-param ()
	(setq *straight-line-length* 100)
	(setq *robot-width* 10)
	(setq *square-width* 50)
	(if (probe-file *map-file*) ;; check if the map-file exists
		(let* ((stream (skippy::load-data-stream 
			(if (string= (subseq *map-file* (- (length *map-file*) 4) (length *map-file*)) ".png") (convert-to-gif *map-file*) *map-file*)))
			(image (aref (skippy::images stream) 0))
			(canvas-width (skippy::width image))
			(canvas-height (skippy::height image)))
			(setq *actr-width* canvas-width)
			(setq *actr-height* canvas-height))
		(progn
			(setq *actr-width* 0)
			(setq *actr-height* 0))))

;; the function that judges if two points are equal
(defun point-equal (p1 p2)
	(and (= (car p1) (car p2)) (= (cdr p1) (cdr p2))))

;; the function that starts the predict process
(defun start-predict ()
	;; initialize params
	(setq *robot-current-pos* nil)
	(setq *robot-previous-pos* nil)
	(setq *robot-goal-pos* nil)
	;; main loop
	(loop while t
		;; end condition
		; until (and *robot-current-pos* *robot-goal-pos* (< (vectors::point-distance *robot-current-pos* *robot-goal-pos*) 10))
		do 
		(progn
			(setq *robot-goal-pos* (read-tcp "actr.goal"))
			(setq pose (read-tcp "actr.pose"))
			(if *robot-goal-pos* 
				;; if goal is not nil, do something
				(if *robot-goal-history*
					;; if the goal history is not nil
					(progn
						(setq prev-goal (car (last *robot-goal-history*)))
						(if (point-equal prev-goal *robot-goal-pos*)
							;; if the goal is the same as the previous goal
							(if *robot-current-pos* 
								;; if *robot-current-pos* is not nil
								(if (point-equal *robot-current-pos* pose)
									;; if pose equals *robot-current-pos*, the robot is no longer moving, do not do the ACT-R planning
									(progn
										(print (concatenate 'string "pose: " (format nil "~a" pose)))
										(print (concatenate 'string "*robot-current-pos* " (format nil "~a" *robot-current-pos*)))
										(print "stand by."))
									;; do the ACT-R planning, and compare the pose with the plan (if pose is different from *robot-current-pos*)
									(if (probe-file *map-file*) ;; check if the map-file exists
										(progn 
											(if (and (= *actr-width* 0) (= *actr-height* 0))
												(set-param))
											(vecto::with-canvas (:width *actr-width* :height *actr-height*)
												(vecto::with-graphics-state
													(graphical-solve *map-file* (trans-coor *robot-current-pos*) (trans-coor *robot-goal-pos*))))
											(and *visual-trace-path*
												(decide (trans-coor pose))
												(trigger))
											(setq *robot-previous-pos* *robot-current-pos*)
											(setq *robot-current-pos* pose)))
										)
								;; else, do not do the planning when *robot-current-pos* is not assigned
								(progn 
									(setq *robot-previous-pos* *robot-current-pos*)
									(setq *robot-current-pos* pose)))
							;; else, the goal is not the same as the previous goal
							(progn
								(setq *robot-goal-history* (append *robot-goal-history* (list *robot-goal-pos*))) ;; add the new goal to the history
								(setq *robot-current-pos* pose) ;; update the current pos
								(setq *visual-trace-path* nil) ;; empty the previous plan
								)))
					;; else, the goal history is nil, simply add the goal to history
					(progn
						(setq *robot-goal-history* (list *robot-goal-pos*))
						(setq *robot-current-pos* pose)))					
				;; if goal is nil, just print "not started"
				(print "not started."))
			(sleep 1))))