(load "find-path.lisp")

(defun run-over-geo-files ()
  (asdf:oos 'asdf:load-op 'cl-ppcre)
  
  (let* ((dirpath "/Users/dr/path-data/part2/*Mid.log")
	 (stats-output (open "/Users/dr/MURI/path/part1/model-results.txt" 
			    :direction :output :if-exists :supersede :if-does-not-exist :create))
	(files  (directory (pathname dirpath)) )) ;; (glob "*.png")))

    ;; note: we used to have :if-exists :overwrite  (for SMC paper submitted ms.)!!!
    ;; this may have caused faulty output -?

    (format stats-output "subj cond time max.straight bot.width subj.norm.area area plen.subj plen.model png ~%")

;    (loop for *robot-width*

    (setq  *robot-width* 48) ; twice the normal width

  ;  (loop for *straight-line-length* from 40 to 400 by 40 do
;	 (let ((write-png (eq *straight-line-length* 400)))
    (let ((write-png t))
       (let ((*straight-line-length* 320))  ;;  320 seems to be good
	 (loop for log-pathname in files do

	 (let* ((file (file-namestring log-pathname))
		(file-path (directory-namestring log-pathname))
		(id (subseq file 0 (- (length file) 7))))

	   (ppcre::register-groups-bind (subj cond) ("No([0-9]+)_([0-9]+)Mid" file)
	
	     (print *straight-line-length*)
	     (print subj)
	     (print cond)
	   (let* ((pixel-size nil)
		  (fp (open log-pathname :direction :input))
		  (time-sc (ppcre::create-scanner "^[0-9]+"))
		  (sc (ppcre::create-scanner "RGoto\\[x=([0-9-\.]+),\\s+y=([0-9-\.]+)")))
	   
	     (let ((itineraries
		    (loop
		       for line-content = (read-line fp nil 'end-of-file)
		       until (eq line-content 'end-of-file)
		       when (search "RExpPlan" line-content)
		       collect
		       (let* ((waypoints nil)
			      (time (ppcre::scan-to-strings time-sc line-content))
			      (geo-data (read-geo-data file-path id time)))
		    
			 (setq pixel-size (first geo-data))
			 (ppcre::do-register-groups (ystr xstr)
			     (sc line-content)
			   (let ((x (/ (- (read-from-string xstr) (second geo-data)) pixel-size))
				 (y (/ (- (read-from-string ystr) (third geo-data)) (- pixel-size))))
			     (push (cons x y)
				   waypoints)))
			 (cons time (reverse waypoints))))))


	       ;; for each itinerary
	     
	       ;; visualize path 
	       (loop for (time . waypoints) in itineraries 
		  ; when (equal time "1214926578406")
		    when (> (length waypoints) 1) ;; ignore pure waypoint instructions
		  do
			 
		    (let* ((image-file  (convert-to-gif (format nil "~a~a_~a.png" file-path id time)))
			   (stream (skippy::load-data-stream image-file))
			   (image (aref (skippy::images stream) 0))
			   ;; for flip-point
			   (*canvas-width* (skippy::width image))
			   (*canvas-height* (skippy::height image)))
			  
		   ;   (print (skippy::color-table-entry (skippy::color-table image) 0))
		    ;  (print (skippy::color-table-entry (skippy::color-table image) 1))
		     ; (print (skippy::color-table-entry (skippy::color-table image) 2))
		      (vecto::with-canvas (:width (skippy::width image) :height (skippy::height image))
			(vecto::with-graphics-state
			  (print time)
			  (print waypoints)
			  (when write-png
			    (visualize-path (mapcar #'flip-point waypoints) nil *yellow*))

			  ;; find 
			  (graphical-solve image-file (flip-point (car waypoints)) (flip-point (car (last waypoints))))
			  (when write-png
			    (visualize-path *visual-trace-path* nil *green*)

			    (mark-point (flip-point (car waypoints)) *green*)
			    (mark-point (flip-point (car (last waypoints)))  *red*))

			; calculate area
			(let ((area-between-paths
			       (let ((state (aa::make-state)) (area 0.0)  )
				 (loop 
				    for knots on (append *visual-trace-path*  (mapcar #'flip-point waypoints) (list (car *visual-trace-path*)))
				    do
				      (if (second knots)
					  (aa::line-f state (car (first knots)) (cdr (first knots)) (car (second knots)) (cdr (second knots)))))
				 (aa::cells-sweep state 
						  (lambda (_x1 _y _a) )
						  (if write-png
						      (lambda (x1 x2 y alpha)
							(draw-line (cons x1 y) (cons x2 y))
							(setq area (+ area (abs (- x1 x2)))))
						      (lambda (x1 x2 y alpha)
							(setq area (+ area (abs (- x1 x2)))))))
				 area))
			      (area-under-subject-path ; polygon spanned by subject path and direct start-end route
			       (let ((state (aa::make-state)) (area 0.0)  )
				 (loop 
				    for knots on (append (mapcar #'flip-point waypoints) (list (flip-point (car waypoints))))
				    do
				      (if (second knots)
					  (aa::line-f state (car (first knots)) (cdr (first knots)) (car (second knots)) (cdr (second knots)))))
				 (aa::cells-sweep state 
						  (lambda (_x1 _y _a) )
						  (lambda (x1 x2 y alpha)
						    (setq area (+ area (abs (- x1 x2))))
						    ))
				 area)))

				
			  (let ((len-wp (waypoints-len waypoints))
				(len-vis (waypoints-len *visual-trace-path*))
				(target-file (format nil "~a~a_~a-waypoints.png" file-path id time)))
			     
			    (when (and (> len-wp 0) (> len-vis 0))
			      (format stats-output "~a ~t ~a ~t ~a ~t ~a ~t ~a ~t ~a ~t ~a ~t ~a ~t ~a ~t ~a~%" 
				      subj cond time (* *straight-line-length* pixel-size) 
				      (* *robot-width* pixel-size)
				      (* area-under-subject-path (expt pixel-size 2))
				      (* area-between-paths (expt pixel-size 2)) 
				      (* len-wp pixel-size)
				      (* len-vis pixel-size)
				      target-file)
			      (force-output stats-output)
			      (when write-png
				(vecto::save-png *temp-file*)
				(compose-images image-file *temp-file* target-file))))

			;; (display-image *temp-file*)
			))))))
	     (close fp)
	   ))))))
    (close stats-output)))


;(run-over-geo-files)


;(make-maze)

