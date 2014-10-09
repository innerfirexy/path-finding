;; Define the function that takes input from json, and predict whether the current robot location is out of the expected route
;; Yang Xu
;; 9/29/2014

(defparameter *robot-current-pos* nil)
(defparameter *robot-previous-pos* nil)
(defparameter *robot-goal-pos* nil)
(defparameter *output* nil)
(defparameter *pos-file* "/usr/local/Cellar/glassfish/4.0/libexec/glassfish/domains/domain1/docroot/era_actr_pose.txt")
(defparameter *goal-file* "/usr/local/Cellar/glassfish/4.0/libexec/glassfish/domains/domain1/docroot/era_actr_goal.txt")
(defparameter *map-file* "./t3v1.png")
(defparameter *local-height* 3288)
(defparameter *local-width* 598)
(defparameter *actr-height* 1919)
(defparameter *actr-width* 330)

;; load packages
;(load "./quicklisp/setup.lisp")
;(ql:quickload "cl-ppcre")
;(ql:quickload "cl-vectors")
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
		(unless (string= (subseq return-val 0 9) "ERA:=null")
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
	(setq pos-x (round (* pos-x *local-height* (/ 1 *actr-height*))))
	(setq pos-y (- *local-height* (round (* pos-y *local-height* (/ 1 *actr-height*)))))
	(cons pos-x pos-y)))

;; the function that sets parameters
(defun set-param ()
	(setq *straight-line-length* 100)
	(setq *robot-width* 10)
	(setq *square-width* 50))

;; the function that starts the predict process
(defun start-predict ()
	;; initialize params
	(setq *robot-current-pos* nil)
	(setq *robot-previous-pos* nil)
	(setq *robot-goal-pos* nil)
	;; main loop
	(loop while t
		until (and *robot-current-pos* *robot-goal-pos* (< (vectors::point-distance *robot-current-pos* *robot-goal-pos*) 10))
		do 
		(progn
			(setq *robot-goal-pos* (read-tcp "actr.goal"))
			(setq pose (read-tcp "actr.pose"))
			(when *robot-goal-pos*
				(if *robot-current-pos* 
					;; do the ACT-R planning, and compare the pose with the plan (if pose is different from *robot-current-pos*)
					(progn 
						(graphical-solve *map-file* (trans-coor *robot-current-pos*) (trans-coor *robot-goal-pos*))
						(and *visual-trace-path*
							(or (/= (car *robot-current-pos*) (car pose)) (/= (cdr *robot-current-pos*) (cdr pose)))
							(decide (trans-coor pose))
							(trigger))
						(setq *robot-previous-pos* *robot-current-pos*)
						(setq *robot-current-pos* pose))
					;; else, do not do the planning when *robot-current-pos* is not assigned
					(progn 
						(setq *robot-previous-pos* *robot-current-pos*)
						(setq *robot-current-pos* pose))))
			(unless *robot-goal-pos* (print "not started."))
			(sleep 1))))



;; read the position every 1 sec
; (loop while t 
; 	do 
; 	(progn 
; 		(setq pose (read-file *pos-file*))
; 		(setq *robot-goal-pos* (read-file *goal-file*))
; 		;; decide whether to trigger
; 		(and *visual-trace-path*
; 			(or (/= (car *robot-current-pos*) (car pose)) (/= (cdr *robot-current-pos*) (cdr pose)))
; 			(decide pose)
; 			(trigger))
; 		(unless (decide pose) (print "not trigger!"))
; 		;; update the position and plan
; 		(setq *robot-previous-pos* *robot-current-pos*)
; 		(setq *robot-current-pos* pose)
; 		(graphical-solve *map-file* *robot-current-pos* *robot-goal-pos*)
; 		;; sleep for 1 sec
; 		;(sleep 1)
; 		))