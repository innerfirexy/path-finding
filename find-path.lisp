;; VISUAL & MEMORY-BASED PATH FINDING IN MAZES AND RASTER GRAPHICS FILES
;; (C) David Reitter and Christian Lebiere
;; 2009, Carnegie Mellon University


;; libraries needed:
;; CL-VECTOR, VECTO, SKIPPY  (install using ASDF)
;; ImageMagick

;; Configure:
;; set paths to libraries and input/output files in appropriate places
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ENTRY POINTS

;; (run-over-geo-files)
;; configure paths in that function
;; IEEE-SMC experiments (Lewis data) - logs from Mrcs experiment

;; (make-maze)
;; ignore the error

;; (graphical-solve image-file start end)
;; MUST follow the instructions:
;; (make-maze)
;; (graphical-solve "demo.png" (cons 300 300) (cons 600 600))
;; visual path finding for image file (gif), from start to end point


;; (run-experiment-1)
;; BRIMS experiments


;; (run-integrated-model)
;; runs graphical+memory-based path finder
;; (not published, not tested much)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TO DO
;; integration with CL's modeL:


;; start without knowledge of the maze.

;; - use memory-driven algorithm.

;; - when forward and backward fail, give up immediately and invoke visual planning algorithm, which plans only until the next stop.
;; - this should store each path step for later retrieval


;; experiment
;; - re-run same maze, perhaps with a filler in between. 
;; - re-run the same maze, but with different start/end points
;;   the visual model alone would not show any improvement
;;   we should probably use larger mazes for that



;; Path drawing 

;; missing:
;; model of local pattern recognition (e.g. we can't jump walls even though there's a hole right in the vicinity)
;; i.e. lack of local goal: the problem is not divided into sub-problems
;; we don't address this primarily because we're coming more from a 3D perspective, i.e. 

;; general claim:
;; path planing uses information from the recognition of shapes to take shortcuts.
;; here, we look at straight lines as the basic shape.
;;


(declaim (optimize (speed 1) (space 0) (debug 3)))



(defparameter *solution-found* 'invalid)
(defparameter *graphical-solve* t) 
(defparameter *display-results* 'show)
(defparameter *show-lines-of-sight* nil)
(defparameter *show-start-end-cells* t)

(defparameter *robot-width* 1)  ; 0.60m/0.025  24 for Usarsim
(defparameter *visual-num-finsts* 5)
(defparameter *visual-attention-latency* 0.085)

(defparameter *straight-line-length* 150)

(defparameter *solvable-maze* t)

(defparameter *square-width* 20)

(defparameter *density-sampling* nil) ;; on for IEEE-SMC experiment

(defparameter  *heuristic-game-over-exp* 1.5) ;; 1 for Usarsim exp

(defvar *visual-trace-path* nil)
(defvar *visual-trace-path-meta* nil)

(defparameter *wall-ratio* 4) ; 5 for  visual-only maze experiments



;; use (make-maze) to make a maze.
 "Makes a maze.
CELLS-H specifies width, CELLS-V specifies height.
Returns a list of the form (start-pos end-pos top-nodes).
The two -pos elements are lists of the form (x y node).
A node is of the form (name size . node-list), whereas
node-list is a list of nodes reachable from node.  
The resulting graph is circular."

(setf *print-circle* T)

(load "actr5.lisp")

(load "graph-processing.lisp")
(load "high-level planning.actr")

(load "./quicklisp/setup.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
(load "./cl-vectors-0.1.5/paths-package.lisp")
  (load "quicklisp.lisp")
  (ql:quickload 'cl-vectors)
  (ql:quickload 'vecto)
  (ql:quickload 'skippy)
)


(defparameter *temp-file* "/tmp/out.png")
(defparameter *temp-file1* "/tmp/out2.png")
(defparameter *temp-file-gif* "/tmp/out.gif")

(defparameter *red* '(1 0.3 0 0.8))
(defparameter *green* '(0 0.3 0 1.0))
(defparameter *blue* '(0 0 0.8 0.8))
(defparameter *yellow* '(0.7 0.7 0 1.0))
(defparameter *orange* '(1 0.4 0.3 1.0))
(defparameter *grey* '(0.5 0.5 0.5 0.7))

(defun convert-to-gif (filename &optional force)
  (if (equal (subseq filename (- (length filename) 4)) ".gif")
      filename ;; no-op if it's already a GIF
      (let ((target (concatenate 'string (subseq filename 0 (- (length filename) 4)) ".gif")))
	(or (and (not force) (if (probe-file target) target))
	    (progn (format t "Converting ~a to GIF file." filename)
		   (ccl::run-program "convert" (list filename target) :output t )
		   target)))))

(defun display-image (file)
  (ccl::run-program "open" (list "-a" "Preview.app" file) :output t ))

(defun compose-images (background foreground target)
 (ccl::run-program "composite" (list foreground background target) :output t ))


(defun transp (x) x)
(defun flip-point (p)
  (cons (car p) (- *canvas-height* (cdr p))))

(defun waypoints-len (points)
  (let ((plen 0))
    (loop with prev = nil
       for knot in points
       do
	 (when prev
	   (setq plen (+ plen (vectors::point-distance prev knot))))
	 (setq prev knot)
	 ) plen))

(defun choice (sequence)
  (and sequence
       (elt sequence (random (length sequence)))))
 
(defun some-position (element list)
  (choice
   (loop 
      for i from 0 to (1- (length list)) 
      when (equal (nth i list) element) collect i)))


(defun single-cell-in-set (index array)
  (let ((count 0) (set (nth index array)))
    (loop for elt in array do 
	 (if (eq set elt) (incf count)))
    (= count 1)))

(defun union-cell-sets (array i j initial)
  (let ((new-set (nth i array))
	(old-set (nth j array)))
    ;; (print (format nil "unioning ~s and ~s in set ~s" new-set old-set array))
    (loop for x from 0 to (1- (length initial)) do
	 (if (eq (nth x initial) old-set) 
	     (setf (nth x initial) new-set)))
    (loop for elt in array 
       collect (if (eq elt old-set) new-set elt))))

(defun first-common-member (a b)
  (loop for x in a do
       (if (member x b) 
	   (return t))))

(defun acceptable-continuation (initial current right-walls)
  "t if current set is okay given initial set."
  ;; we want at least one set mentioned in the initial to appear in the current
  (and (first-common-member initial current)
       
;;))
;;(defun xxx ()
       ;; we also want at there at least 50% different sets
       ;; adjacent cells without a wall count as 1
       (let ((num 0) (sets nil))
	 (loop for x in current 
	    for wall in right-walls do
	      (and wall (incf num)
		   (not (member x sets))
		   (setq sets (cons x sets))))
	 (or (> (length sets) 4)
	      (> (length sets) (* 0.5 num))))))

      
(defun compress-node (n &optional exclude-nodes)
  (unless (member n exclude-nodes)
    (if (> (length n) 3)
      (loop for n2 in (cddr n) do
	   (compress-node n2 (cons n exclude-nodes)))
      (when (= (length n) 3)
	;; new ID
	 (setf (car n) (intern (concatenate 'string (symbol-name (car n)) "-"  (symbol-name (car (third n))))))
	;; add up length
	(setf (second n) (+ (second n) (second (third n))))
	;; and get rid of second node
	(setf (cddr n) (cddr (third n)))
	;; compress rest
	(compress-node n exclude-nodes)))))

(defun compress-one-node (n)
    (when (= (length n) 3)
	;; new ID
	 (setf (car n) 
	       (intern (concatenate 'string (symbol-name (car n)) "-"  (symbol-name (car (third n))))))
	;; add up length
	(setf (second n) (+ (second n) (second (third n))))
	;; and get rid of second node
	(setf (cddr n) (cddr (third n)))))

(defun location-name (point)
  (intern (format nil "~C~C" (+ 65 (car point)) (+ 66 (cdr point))  )))

(defun name-location (name)
  "decode symbol into x,y location"
  (map 'list #'(lambda (c) (- (char-int  c) 65)) (symbol-name name)))

(defun name-location-cons (name)
  "decode symbol into x,y location"
  (let ((loc (name-location name)))
    (cons (first loc) (second loc))))


(make-random-state t)

(defun draw-new-maze (cell-width cells-h cells-v wall-ratio v-wall-ratio)

  (let* ((cell-height cell-width))
  
      (let ((top-node nil) (node-list nil) (start-pos nil) (end-pos nil))
      (vecto::set-font (vecto::get-font "/Library/Fonts/Courier New.ttf") 18)
    
      (let* ((set-count 100)
	     (cell-set-initial  (loop for i from 0 to cells-h collect i))
	     (cell-set-current cell-set-initial)
	     (node-list-current))
	(setq node-list-current 
	      (loop for i from 0 to cells-h collect
		   (copy-list `(,(intern (format nil "~C~C" (+ 65 i) 65  )) 1 . nil))))
	(setq top-node `(start 0 . ,node-list-current))

	(loop for y from 0 to cells-v do
	     (loop for attempt from 1 to 42 do
		  (let ((right-walls)
			(bottom-walls))
		    (let ((cell-set-current-2 (copy-tree cell-set-current))
			  (cell-set-initial-2 (copy-tree cell-set-initial)))
		      (loop for x from 0 to cells-h do
			 ;; cells
			 ;; determine wall/no-wall for right
			   (let ((connect (and (< x cells-h) ;; rightmost border 
					       (or
						;; special rules for last row
						;; lead to rather boring last rows...
						;; 				   (if (= y cells-v) ; in the last row
						;; 				       ;; must connect if not already in the same set
						;; 				       (not (= (nth x cell-set-current-2) (nth (+ 1 x) cell-set-current-2))))
						(and
						 (> (random 10) wall-ratio) ;; right wall?
						 ;;(not cell-set-previous)
						 ;; do not connect if cells are already in same set:
						 ;; (not (= (nth x cell-set-current-2) (nth (+ 1 x) cell-set-current-2))))
						 )
						))))

			     (setq right-walls (cons 
						(not connect)
						right-walls))
			     (if connect
				 (setq cell-set-current-2 
				       (union-cell-sets cell-set-current-2 
							x (+ x 1) 
							cell-set-initial-2)))))
		      (loop for x from 0 to cells-h do
			 ;; determine wall/no-wall for bottom
			   (let ((connect (or ;; must connect if it's the only cell in its set
					   (single-cell-in-set x cell-set-current-2)
					   (> (random 10) v-wall-ratio))))
		
			     (setq bottom-walls (cons 
						 (not connect)
						 bottom-walls))
			     (unless (or connect (= y cells-v))
			       (setf (nth x cell-set-current-2) (incf set-count)))))
		
		      (when (or (> attempt 40)  
				(acceptable-continuation cell-set-current-2 
							 cell-set-initial-2 right-walls))
			(setq cell-set-current cell-set-current-2)
			(setq cell-set-initial cell-set-initial-2)
       
			(setq bottom-walls (reverse bottom-walls))
			(setq right-walls (reverse right-walls))

			;; write nodes
			(let ((node-list-next))
			  (loop for next-node in (append (cdr node-list-current) (list nil)) ;; node to the right 
			     for node in node-list-current
			     for rw in right-walls
			     for bw in bottom-walls 
			     for xi from 0 to cells-h
			     do 
			     ;; connect horizontal neighbors
			     (unless rw
			       (when next-node
				 (setf (cdr (last node)) (list next-node))
				 (unless (member node (cddr next-node))
				   (setf (cdr (last next-node)) (list node)))))
			     ;; create nodes for the row below
			       (unless (= y cells-v)
				 (let ((new-node (copy-list `(,(location-name (cons xi y)) 1 . nil))))
				   (setq node-list (cons new-node node-list))
				   (setq node-list-next (append node-list-next (list new-node)))
				   (unless bw
				     (setf (cdr (last node)) (list new-node))
				     (unless (member node (cddr new-node))
				       (setf (cdr (last new-node)) (list node))
				       )))))
			  (unless (= y cells-v)
			    (setq node-list-current node-list-next)))

			;; drawing
			(loop for x from 0 to cells-h do
			   ;; (vecto::draw-string (* cell-width (+ x 0.5))
;; 			      (- *canvas-height* (* cell-height (+ y 0.5)))
;; 			      (format nil "~C~C" (+ 65 x) (+ 65 y)  ))
			   ;; 	  (format nil "~s" (nth x cell-set-current) ))
			     (when (nth x right-walls)
			       (vecto::move-to  (* cell-width (+ x 1)) 
						(transp (* cell-height y)))
			       (vecto::line-to  (* cell-width (+ x 1)) 
						(transp (* cell-height (+ y 1)))))
	      
			     (when (nth x bottom-walls)
			       (vecto::move-to  (* cell-width (+ x 0)) 
						(transp (* cell-height (+ y 1))))
			       (vecto::line-to  (* cell-width (+ x 1))  
						(transp (* cell-height (+ y 1))))))
			(return t))))))




	(setq start-end-positions
	      (loop for solution-set in  (intersection cell-set-initial cell-set-current) collect

		   (let* ((x (or nil (some-position solution-set cell-set-initial)))
			 (x2 (or nil (some-position solution-set cell-set-current)))
			 (x-alt  (some-position (print (choice (mapcan (lambda (xa) (unless (eq solution-set x) (list xa))) cell-set-current)))
						cell-set-current))) 
		     
		     (cons (list x 0 (nth x (cddr top-node))) ; start-pos
			   (list (if *solvable-maze* x2 x-alt) cells-v (nth (if *solvable-maze* x2 x-alt) node-list-current)) ; end-pos
			   )
		     ;; 	       (setq start-pos (list x 0 (nth x (cddr top-node))))
		     ;; 	       (setq end-pos (list (if *solvable-maze* x2 x-alt) cells-v (nth (if *solvable-maze* x2 x-alt) node-list-current)))
		     
		     ))))
      (let ((some-challenge (choice start-end-positions)))
	(setq start-pos (car some-challenge) 
	      end-pos (cdr some-challenge)))

      
      (vecto::rectangle  0 0 ;;(transp (* cell-height (+ 1 cells-v)))
			 (* cell-width (+ 1 cells-h))  (* cell-height (+ 1 cells-v)))
      (vecto::stroke)

      (when *show-start-end-cells*
	(vecto::set-rgba-fill 1.0 0 0 0.2)
	(mark-maze-cell start-pos)
	(mark-maze-cell end-pos))

      (vecto::save-png *temp-file*)

      (list start-pos end-pos top-node (convert-to-gif *temp-file* t) start-end-positions)
))
)

(defun mark-maze-cell (pos)
  (vecto::rectangle  (* *square-width* (+ 0.0 (car pos))) (transp (* *square-width* (+ (second pos) 0.0)))
		     (* 1.0 *square-width*)  (* 1.0 *square-width*))
  (vecto::fill-path))


(defun make-maze (&optional cells-h cells-v cell-size)
  "Makes a maze.
CELLS-H specifies width, CELLS-V specifies height.
Returns a list of the form (start-pos end-pos top-nodes).
The two -pos elements are lists of the form (x y node).
A node is of the form (name size . node-list), whereas
node-list is a list of nodes reachable from node.  
The resulting graph is circular."

  (setq cells-h (or cells-h 10))
  (setq cells-v (or cells-v 10))

  (setq *solution-found* 'invalid)

  (let* ((cell-width (or cell-size 20)) (cell-height (or cell-size 20))
	 (*canvas-width* (* cell-width (+ 2 cells-h)))
	 (*canvas-height* (* cell-height (+ 2 cells-v)))
	 (wall-ratio *wall-ratio*)	;; out of 10
	 (v-wall-ratio *wall-ratio*)
	 (start-end-top nil)
	
	 )
    (setq *square-width* cell-height)
    (vecto::with-canvas (:width *canvas-width* :height *canvas-height*)
      (setq start-end-top (draw-new-maze cell-size cells-h cells-v wall-ratio v-wall-ratio))

      (when *graphical-solve*
      (setq *solution-found*
	    (graphical-solve (nth 3 start-end-top) ; GIF name
			     (cons (* cell-width (+ 0.5 (car (first start-end-top))))  
				   (transp (* cell-height (+ (second (first start-end-top)) 0.5))  ))
			     (cons (* cell-width (+ 0.5 (car (second start-end-top))))  
				   (transp (* cell-height (+ (second (second start-end-top)) 0.5))  ))
			     ))
      (when *display-results*
	(visualize-path *visual-trace-path* t)
	
	(vecto::save-png *temp-file*)
	
	(when (eq *display-results* 'show)
	  (display-image *temp-file*))
	 )))

    ;; (mapc 'compress-one-node node-list)
  start-end-top))

  
; (make-maze)
  

(defun mark-point (p &optional col)
  (vecto::with-graphics-state
  (setf (vecto::paths vecto::*graphics-state*) nil
          (vecto::path vecto::*graphics-state*) nil)
  (if col
      (apply #'vecto::set-rgba-stroke col))
  (vecto::set-line-width 1)
  (vecto::move-to  (- (vectors::point-x p) 5) (transp (- (vectors::point-y p) 5)))
  (vecto::line-to (+ (vectors::point-x p) 5) (transp (+ (vectors::point-y p) 5)))
  (vecto::stroke)
  (vecto::move-to  (- (vectors::point-x p) 5) (transp (+ (vectors::point-y p) 5)))
  (vecto::line-to (+ (vectors::point-x p) 5) (transp (- (vectors::point-y p) 5)))
  (vecto::stroke)))

(defun mark-dot (p &optional col)
  (vecto::with-graphics-state
  (setf (vecto::paths vecto::*graphics-state*) nil
          (vecto::path vecto::*graphics-state*) nil)
  (if col
      (apply #'vecto::set-rgba-stroke col))
  (vecto::set-line-width 1)
  (vecto::move-to  (- (vectors::point-x p) 1) (transp (- (vectors::point-y p) 0)))
  (vecto::line-to (+ (vectors::point-x p) 0) (transp (+ (vectors::point-y p) 1)))
  (vecto::stroke)))


(defun angle-difference (a1 a2)
(abs (- (mod a1 (* pi 2))
	(mod a2 (* pi 2)))))

(defun perpendicular-norm (from to &optional length)
  "Perpendicular vector of length 1 or LENGTH"
  (if (equal from to)
      (vectors::make-point 0 0)
      (let* ((p (vectors::make-point (- (vectors::point-y from) (vectors::point-y to))
				     (- (vectors::point-x to) (vectors::point-x from)))))
	(vectors::p* p (/ (or length 1) (vectors::point-norm p))))))


(defun mapcons (function cons)
  (cons (funcall function (car cons)) (funcall function (cdr cons))))

(defun cons-member (cons cons-list &optional max-index)
  (unless max-index (setq max-index 999999999999999))
  (loop for index from 1 to max-index 
     for elt in cons-list do
       (and (equal (car elt) (car cons))
	    (equal (cdr elt) (cdr cons))
	    (return t))))

(defun rad (deg)
  (/ (* deg 2 3.14159265) 360))

(defun deg (rad)
  (  * 360 (/ rad 3.14159265 2)))

(defvar *visual-attention-loc* nil)

(defvar *canvas-height*)

(defvar *visual-finsts* nil)

(defvar *solution-squares* nil)

(defun graphical-solve (image-file start end)
  (declare (optimize (debug 2)))

  (init-cog-model)
  
  (setq *solution-squares* nil)

  (walk-to start)
;  (walk-to (cons 300 300))
  
  (let* (
	 (stream (skippy::load-data-stream (convert-to-gif image-file)))
	 (image (aref (skippy::images stream) 0))
	 (canvas-width (skippy::width image))
	 (canvas-height (skippy::height image))
	 ;; (canvas (skippy::canvas-image image))
	 )
    (setq *canvas-height* canvas-height)
    ;; start with direct line
    ;(vecto::set-rgb-stroke 0 1 0)
    ;(vecto::set-line-width 2)
    (loop for step from 0 to (expt (/ canvas-width *square-width*) *heuristic-game-over-exp*) ;; heuristic
       until (or (equal (point-square end) (point-square *visual-attention-loc*))
		 (< (vectors::point-distance end *visual-attention-loc*) *square-width*))
       do
	 (move-to-next-target image end))
    ;; did we reach the goal?
    (equal (point-square end) (point-square *visual-attention-loc*))
    ))

(defun move-to-next-target (image end)
  (let* ((goal-angle (vectors::point-angle (vectors::p- end *visual-attention-loc* )))
	      (angle (+ (* -2 pi) (* -1 goal-angle))))
	 ;;    (vecto::rotate 0.2) ; angle)
	 ;; rotate doens't seem to work well
   
	 (let ((best-angle nil) (best-length 999999) (best-target) (best-square) (best-debug-points)
	       (bext-x) (best-y) (best-location))
	   (loop for xd  in '(0) for yd in '(0) ;in (list 0.5 -0.5 0.5 -0.5) for yd in (list -0.5 0.5 0.5 -0.5) 
	
	      do
	      (let ((location  (cons (+ (car *visual-attention-loc*) xd) 
				     (+ (cdr *visual-attention-loc*) yd))))
		(loop for angle from (+ angle (* -2 pi)) to (+ angle (* 2 pi)) by (/ pi 20) do
		  
		     (let ((xdisp (* *straight-line-length* (sin (/ angle 1)))) ;; calculate straight line at angle
			   (ydisp (* *straight-line-length* (cos (/ angle 1)))))

		       (vecto::with-graphics-state
		      ; 	(setf (vecto::*graphics-state*) nil (vecto::paths vecto::*graphics-state*) nil
        	   	;	(vecto::path vecto::*graphics-state*) nil)
		       	;(vecto::set-rgb-stroke 0 1 0)
    			;(vecto::set-line-width 2)
    

			 ;; construct the path
			 (vecto::move-to (car location)  (transp (cdr location)))
			 (vecto::line-to (+ (car location) xdisp) (transp (+ (cdr location) ydisp)))
			
			 (let* ((target-point (longest-unobstructed-path-segment 
					       (vecto::path vecto::*graphics-state*) 
					       image
					       (/ *square-width* 2)
					       end 2))
				(goal-dist (if target-point (vectors::point-distance end target-point) 0))
				(len (if target-point (vectors::point-distance location target-point) 0))
				(square (if target-point (point-square target-point)))
				(length-scaled (if *density-sampling* (sample-path-density image target-point end)
						   (+ (* 1 goal-dist) (* 0 len)))) ;;)
				)
			   ;; illustrate lines-of-sight
			   (and target-point
				*show-lines-of-sight*
				(draw-line location target-point *green*))
			   ;;(mark-point target-point)

			   (when (and target-point
				      goal-dist 
				      ;; only avoid going back when there's really nothing better around
				      (or (not (finst-previously-visited square))  ; to do: use 
					  ;; if we're backtracking, add a penalty in order to avoid it 
					  (setq length-scaled (+ length-scaled 1000)))
				      (or (> len  *square-width*) ;; force movements
					  (and (< goal-dist (* 3 *square-width*)) (> len  2))
					  (setq length-scaled (+ length-scaled 100))
					  )
				      (< length-scaled best-length))
			     (setq best-angle angle best-length length-scaled
				   best-x xdisp best-y ydisp best-target target-point best-square square
				   best-location location best-debug-points debug-points-list))))))))
 ;; 	   (draw-line (cons (car best-location)  (transp (cdr best-location)))
;;  		      (cons (+ (car best-location) best-x) (transp (+ (cdr best-location) best-y)))
;;  		      *red*)

	   (when best-target
	     (print 'best)
	     (print best-target)
	     (print best-square) (print best-length)
	     (when (> (angle-difference best-angle goal-angle) pi)
	       (print 'backtracK)
	       (print best-angle)
	       (print goal-angle)
	       )
	     ;; (loop for p in best-debug-points do (mark-dot p *red*))
	     (walk-to best-target (> (angle-difference best-angle goal-angle) pi) image end))
      
	 (setq best-target nil)
	 )))

(defun sample-path-density ( skippy-image start end)
  "Samples within a thick rectangle over and paralleling the line start-end."

  
  (let* ((sample-density 0.1)
	 (obstruction-weight 1)
	 (spp (perpendicular-norm start end))
	 (dir (vectors::p- end start))
	 (len (vectors::point-norm dir))
	 (canvas-height (skippy::height skippy-image))
	 (canvas-width (skippy::width skippy-image))
	 ) ;; length start-end
    (+ len (* obstruction-weight len sample-density 
    (/
     (loop for i from 0 to 1 by sample-density sum
	  (let ((point (vectors::p+ start (vectors::p* dir i))))

	    (loop for j from -40 to +40 by 4 sum
		 (let* ((point2 (vectors::p+ start (vectors::p* spp j)))
			(col (if (and (< (car point2) canvas-width) (< (cdr point2) canvas-height) (> (car point2) 0) (> (cdr point2) 0))
				 (skippy::pixel-ref skippy-image (round (car point2)) (round (- canvas-height (cdr point2))))
				 0 ; black
				 )))
		   ;; 0 -black
		   ;; 1 - blue
		   ;; 2 - white
		   ;; (mark-dot point2)
		   (cond ((eq col 0) 1)
			 ((eq col 1) 0.05)
			 ((eq col 2) 0)
			 (t 0))))))
     20) ;; scales linearly with length, but let's normalize by width
    ))))

(defun walk-to-old (point)
  (declare (optimize (debug 2)))
  (print point)
  (mark-point point *red*)
  (if *visual-attention-loc*
      (vecto::with-graphics-state
	(vecto::set-line-width 1)
	(vecto::set-rgb-stroke 1 0 0)
	(draw-line *visual-attention-loc* point)
	))
  (setq *visual-attention-loc* point))

(defun point-square (point)
  (if point (mapcons #'floor (vectors::p* point (/ 1 *square-width*)))))

(defun point-previously-visited (point)
  (finst-previously-visited (point-square point)))

(defun square-point (x y)
 (vectors::p* (vectors::p+ (vectors::make-point x y) (vectors::make-point 0.5 0.5)) *square-width*))


;; visual system
(defun finst-previously-visited (loc)
  (cons-member loc *visual-finsts* *visual-num-finsts*))


(defparameter *screen-resolution* 34) ;; pixels per centimater
(defparameter *distance-from-screen* 50) ;; centimeters

(defparameter *navigation-mode* nil)
(defun shift-visual-attention (loc)
  (if *visual-attention-loc*
      (let* ((pixel-distance (vectors::point-distance *visual-attention-loc* loc))
	     ;; eccentricity distance
	     (theta (atan (/ (/ pixel-distance  *screen-resolution*) *distance-from-screen*)))
	     (encoding-time (* (- (log 0.5)) (exp theta))))
;; tan(theta) = sin(theta) / cos(theta) = a / b
 
;; encoding time
;;	 Tenc = c * (- log f) * exp(theta)
;; f = frequency from 0 to 1
      
	(pass-time encoding-time)))
      
    
  (setq *visual-attention-loc* loc)    
  (setq *visual-trace-path* (cons loc *visual-trace-path*))
  (setq *visual-trace-path-meta* (cons *navigation-mode* *visual-trace-path-meta*))
  ;; add this to the recent point history
  (add-visual-finst (point-square loc)))

(defun add-visual-finst (loc)
  (unless (equal (car *visual-finsts*) loc)
    (setq *visual-finsts* (cons loc *visual-finsts*))))

  

(defun walk-to (point &optional backtracking skippy-image end)
  (setq point (mapcons #'round point))
  
  (print (format nil "walking to: ~s" point))
  
  ;; between old and new point, record all the locations in between
   
  (let* ((pts nil)
	 (end-sq  (point-square end) )
	 (space-samples)
	 (dist (if  *visual-attention-loc* (vectors::point-distance *visual-attention-loc* point))))
    ;; collect points
    (when (and  *visual-attention-loc* (> dist 0))
      (loop for i from 0 to 1 by (max 0.05 (/ *square-width* 2 dist))
	 ;;from 0 to (vectors::point-distance *visual-attention-loc* point)
	 with dir = (vectors::p- point  *visual-attention-loc*)
	 with spp = (perpendicular-norm *visual-attention-loc* point)
	 collect  (vectors::p+ *visual-attention-loc* (vectors::p* dir i))
	 do

	 (let* ((delta (vectors::p* dir i))
		(current (vectors::p+ *visual-attention-loc* delta))
		(sq (point-square current)))

	   (when (equal sq end-sq)
	       ;; finish reached
	       (setq point current)
	       (return))

	   ;; (mark-point current)
	   ;; we're looking left and right to see if we're at a corner
	   ;; a corner is recognized iff there is more space (line of sight) than what we sampled on average
	   ;; during the last few moves.

	   (when t ;; (and backtracking (> (vectors::point-norm delta) *square-width*))  ;; are we going towards the goal?

	       ;; checking the finsts would lead us to think we're backtracking most of the time
	       ;; given that places visited are immediately turned into known territory.
	       ;; (cons-member sq *visual-finsts*) 
	     (let* ( ;; let's look left and right to see if there are exists worth exploring
		    (next-wall-left (longest-unobstructed-path-segment
				     (let ((p (vectors::create-path :open-polyline)))
				       (vectors::path-reset p current)
				       (vectors::path-extend p (vectors::make-straight-line)
							     (vectors::p- current (vectors::p* spp (* 10 *square-width*) )))
				       p)
				     skippy-image 10 
				     end 2
				     'fast
				     ))
		    (next-wall-right (longest-unobstructed-path-segment
				      (let ((p (vectors::create-path :open-polyline)))
					(vectors::path-reset p current)
					(vectors::path-extend p (vectors::make-straight-line)
							      (vectors::p+ current (vectors::p* spp (* 10 *square-width*) )))
					p)
				      skippy-image 10
				      end 2
				      'fast))

		    (dist-left (vectors::point-distance current next-wall-left))
		    (dist-right (vectors::point-distance current next-wall-right))
		    )
	       (when (or
		    (and next-wall-left (> dist-left *square-width*)
			 (not (point-previously-visited next-wall-left)))
		    (and next-wall-right (> dist-right *square-width*)
			 (not (point-previously-visited next-wall-right))))
;		 (mark-point current)
	     ;(vecto::set-line-width 1)
	     ;; (draw-line (vectors::p- current (vectors::p* spp (* 3 *square-width*) )) (vectors::p+ current (vectors::p* spp (* 3 *square-width*) )) *blue*)
		 (let ((stats (moving-meandev space-samples))
			 )
		   (when stats ; (and left-stats right-stats)
		     ;; if space to left or right is greater than one standard deviation from the moving average
		     (when (> (+ dist-left dist-right) (+ (car stats) (cdr stats) (cdr stats)))
		       (print 'found-opening)
		       (setq point current)
		       ;; stop walking if anything interesting turns up
		       (return)))))
		 (push (+ dist-left dist-right) space-samples)))
	   (unless (equal (car *solution-squares*) sq)
	     (push sq *solution-squares*))
	   ;; add this to the recent point history
	   (unless (equal (car *visual-finsts*) sq)
	     (setq *visual-finsts* (cons sq *visual-finsts*)))

	   (setq pts (cons current pts)))))
;    (mark-point point *red*)

    (shift-visual-attention point)

    ;; return lists of points in between and those that are actually interesting
    pts))

(defun moving-meandev (samples)
;; returns (mean . stdev) for first five samples

  (when (and samples (> (length samples) 1))
  (let ((sum 0) (num 0))
    (loop for i from 1 to 5
       for s in samples
       do
	 (setq sum (+ sum s))
	 (incf num))
    (let* ((mean (/ sum num))
	   (stddev
	    (sqrt
	     (/ (loop for i from 1 to 5
		   for s in samples
		   sum
		     (expt (- s mean) 2))
		(1- num)))))  ;; R uses num-1
      (cons mean stddev)))))
;(moving-meandev '(5 3 1 4 8 9 6 3))
;(moving-meandev '(5 3))
;(moving-meandev '(5))


(defun visualize-path (points &optional smooth color types)
;; "types" won't work with "smooth"
  

 (let* ((path (vectors::create-path :open-polyline))
	(first (car points))
	 (second (cadr points))
	(rev (reverse (cddr points)))
	(last (car rev))
	 (penultimate (cadr rev))
	(points2 (cddr rev)))

    (apply #'vecto::set-rgba-stroke (or color *red*))
    (vecto::set-line-width 1)

    (if smooth
	(progn
	  (vectors::path-reset path first)
	  (vectors::path-extend path 
				(vectors::make-catmull-rom second
							   points
							   penultimate)
				last)
	  (setf (vecto::path vecto::*graphics-state*) (vectors::make-discrete-path path)
		(vecto::paths vecto::*graphics-state*) (list (vecto::path vecto::*graphics-state*)))
	  
	  (apply #'vecto::set-rgba-stroke (or color *red*))
	  (vecto::stroke))
	;; not smooth

	
	(loop for (p1 p2) on points 
	   for type in types do
	     (when (and p2 p1) ; skip last one
	       (apply #'vecto::set-rgba-stroke 
		      (cond ((eq type 'memory) *green*)
			    ((eq type 'visual) *orange*)
			    (t (or color *red*))))
	       (vecto::move-to (car p1) (cdr p1))
	       (vecto::line-to (car p2) (cdr p2))
	       (vecto::stroke))))))



(defun draw-line (point-a point-b &optional col)
  (declare (optimize (debug 2)))
   (vecto::with-graphics-state
     (setf (vecto::paths vecto::*graphics-state*) nil
	   (vecto::path vecto::*graphics-state*) nil)
     (if col
	 (apply #'vecto::set-rgba-stroke col))

     (vecto::move-to (car point-a)  (transp (cdr point-a)))
     (vecto::line-to (car point-b) (transp (cdr point-b)))
     (vecto::stroke)
    (vecto::set-rgb-stroke 1 1 1)))

(defun transparent-pixel-p (skippy-image x y &optional fast)
;; 0 -black
;; 1 - blue
;; 2 - white
 
  (let ((transp (list 0))
	(x (round x))
	(y (round y)))
    (and (> x 0) (> y 0)
    (if fast
	(not (member (skippy::pixel-ref skippy-image x y)
			  transp))
	(not (and (member (skippy::pixel-ref skippy-image x y)
			  transp) ;;#xD0D0D0)
		  (member (skippy::pixel-ref skippy-image (1+ x) y)
			  transp)
		  (member (skippy::pixel-ref skippy-image x (+ 1 y))
			  transp)
		  (member (skippy::pixel-ref skippy-image (1+ x) (+ 1 y))
			  transp)))))))

(defvar debug-points-list nil)
(defun longest-unobstructed-path-segment (path skippy-image &optional stop-before-wall end-point end-point-radius fast)
  (declare (optimize (debug 2)))
  "Give the longest initial segment of PATH that is probably passable"
  
  (setq  debug-points-list nil)
  
  (block along-the-line
    (let* (
	   
	   (canvas-width (skippy::width skippy-image))
	   (canvas-height (skippy::height skippy-image))
	   (transp (skippy::transparency-index skippy-image))
	   (prev-x ) (prev-y ) 
	   (prev-knot ))
      (loop with iter =
	   (vectors::path-iterator-segmented path)
	   for (interpolation knot end-p) =
	   (multiple-value-list (vectors::path-iterator-next iter))
	   while knot
	   do
	   (when prev-knot
	     
	      	     ;; (vecto::set-line-width 1)
	      	     ;; (draw-line prev-knot knot *grey*)
	  
	     (let* ((len (vectors::point-distance knot prev-knot))
		    (xt  (/ (- (vectors::point-x knot) (vectors::point-x prev-knot)) len)) ;; this many steps
		    (yt (/ (- (vectors::point-y knot) (vectors::point-y prev-knot)) len))
		    (startx (vectors::point-x prev-knot)) 
		    (starty (vectors::point-y prev-knot))
		    (spp (perpendicular-norm prev-knot knot)))
	       (loop for i from 0 to len by (if fast 3 0.3)
		  for x = (+ startx (* xt i)) 
		  for y = (+ starty (* yt i)) do
		  (when prev-x

		    (vecto::with-graphics-state
			(loop for j in (if (and spp *robot-width*)
					   (list (- (/ *robot-width* 2)) (- (/ *robot-width* 4)) 0 
						 (/ *robot-width* 4) (/ *robot-width* 2)) 
					   (list 0)) ;; 5 pixels wide
			   do
			   (let  (( x (+ x  (* j (car spp))))
				  ( y (+ y  (* j (cdr spp)))))

			     (when (and end-point (< (vectors::point-distance end-point (cons x y)) 
						     end-point-radius)) ; (* 1 end-point-radius)
			       (return-from along-the-line (cons x y)))

			     (and (= (abs j) (/ *robot-width* 2))
				  (push (cons x y) debug-points-list))
			     ;; check image
			     (if  (and (> x 2) (< x (- canvas-width 2))
				       (> y 2) (< y (- canvas-height 2)))
				  (unless
				      (transparent-pixel-p skippy-image x (- canvas-height y) fast)
				    ;; mark checkpoint
				    ;; (mark-point (cons (- prev-x (* xt (or stop-before-wall 0))) (- prev-y (* yt (or stop-before-wall 0)))))
				    (return-from along-the-line (cons (- prev-x (* xt (or stop-before-wall 0))) (- prev-y (* yt (or stop-before-wall 0))))))
				  ;; else
				  (when t
				    (if prev-x 
					(return-from along-the-line (cons (- prev-x (* xt (or stop-before-wall 0))) (- prev-y (* yt (or stop-before-wall 0)))))
					(return-from along-the-line nil)
					)))
			     ))))
		  (when (and (> x 0) (< x canvas-width)
			     (> y 0) (< y canvas-height))
		    (setq prev-x x prev-y y)
		    ))))
	   (setq prev-knot knot)
	   (when end-p
	     (when prev-x 
	       (return (cons prev-x prev-y)))
	     (return nil) ;; happens when completely outside boundaries
	     )))))
  
"
always choose the longest unobstructed path segment and move to the end point.
 (add spline points)
MOVE-ALONG-LINE:
  maybe use existing algorithm to move along the line (it should work just fine)
  give start / end points
  so that the object recognition algorithm basically sets intermediate goals
  add choice points to memory stack along the way

if nothing suitable found, backtrack."





;; mini-ACT-R

(defvar *sim-time* 0)
(defvar *memory-stack*)

(defun init-cog-model ()
  (init-dm)
  (init-vis)
  (setq *sim-time* 0))

(defun init-dm ()
  (setq *memory-stack* nil))

(defun init-vis ()
  (setq *visual-attention-loc* nil *visual-trace-path* nil *visual-trace-path-meta* nil *visual-finsts* nil))

(defun remember (object)
  (setq *memory-stack* (cons (cons *sim-time* object) *memory-stack*)))
(defun recall-latest ()
  (let ((dm-chunk (car *memory-stack*)))
    (setq *memory-stack* (cdr *memory-stack*))
    (pass-time (* 1 (exp (- (* 1.0 (bl-activation (car dm-chunk)))))))
    (cdr dm-chunk)))
	

(defun pass-time (msec)
  (setq *sim-time* (+ msec *sim-time*)))

(defun bl-activation (chunk-time)
  (log (- *sim-time* chunk-time))) ;;  rather simplified...


;; to do
;; - generate self-similar or at least regular mazes (tree form?)

;; mazes: we can make mazes more regular, perhaps using patterns
;; buildings: usually they have a tree structure, to some extent
" amicro-planning algorithm:
- try to draw a straight line (thickness 4 pix or so)
  at an angle alpha roughly in the direction of the goal,
  length L.
  vary alpha within reason (160 degrees or so).
  start with low L.
  choose direct line there are no obstructing pixels along the line.
  then, try to extent the line at the same angle. "




(defun solve-maze-cognitively (&optional maze)
  (init-cog-model)
  (setq *solution-squares* nil)

  (let* ((graph (or maze (make-maze)))
	 (from (third (first graph)))
	 (to (third (second graph))))
    (if (eq to (solve-maze-depth-first from to))
	(print 'found)
	(print 'not-found)))
  (print *sim-time*))

      
;; explore 

;; use explicit stack so that we can
;; model memory effects

(defun solve-maze-depth-first (from-node to-node)

  (let ( (previous-node nil) (done nil))
    (remember from-node) ;; put start on stack

    (loop for time from 1 to 500 ; timeout 
       do
	 (pass-time 150) ;; 50 msec
      	 (let ((current-node (recall-latest))) ;; pop
	   (print (car current-node))
	   (when (eq current-node to-node)  ;; check
	     (return current-node))
	   (push current-node *solution-squares*) ;; keep track
	   (push current-node done)  ;; finst
	   (if (cddr current-node) ;; anything to explore?
	     ;; put the new choices on the stack
	       ;; but exclude the one we've come from
	       (loop for choice in (sort (copy-list (cddr current-node)) (lambda (a b) (further a b to-node)))
		    when (not (eq choice previous-node))
		    do
		    (unless (memq choice done)
		      (remember choice)
		      )))
	   (setq previous-node current-node)))))

(defun node-loc (node)
  (cons (- (char-int (char (symbol-name (car node)) 0)) 65) 
	(- (char-int (char (symbol-name (car node)) 1)) 65)))

(defun further (node-a node-b goal)
  "t if node-a is further to goal than node-b"
  
  (let ((al (node-loc node-a))
	(bl (node-loc node-b))
	(gl (node-loc goal)))
    (> (+ (expt (- (car al) (car gl)) 2)
	  (expt (- (cdr al) (cdr gl)) 2))
       
       (+ (expt (- (car bl) (car gl)) 2)
	  (expt (- (cdr bl) (cdr gl)) 2)))))

; graph

;;make-maze


(defparameter *solvable-maze* t)



;; use (make-maze) to make a maze.
  "Makes a maze.
CELLS-H specifies width, CELLS-V specifies height.
Returns a list of the form (start-pos end-pos top-nodes).
The two -pos elements are lists of the form (x y node).
A node is of the form (name size . node-list), whereas
node-list is a list of nodes reachable from node.  
The resulting graph is circular."

(setf *print-circle* T)




(defun choice (sequence)
  (and sequence
       (elt sequence (random (length sequence)))))
 
(defun some-position (element list)
  (choice
   (loop 
      for i from 0 to (1- (length list)) 
      when (equal (nth i list) element) collect i)))


(defun single-cell-in-set (index array)
  (let ((count 0) (set (nth index array)))
    (loop for elt in array do 
	 (if (eq set elt) (incf count)))
    (= count 1)))

(defun union-cell-sets (array i j initial)
  (let ((new-set (nth i array))
	(old-set (nth j array)))
    ;; (print (format nil "unioning ~s and ~s in set ~s" new-set old-set array))
    (loop for x from 0 to (1- (length initial)) do
	 (if (eq (nth x initial) old-set) 
	     (setf (nth x initial) new-set)))
    (loop for elt in array 
       collect (if (eq elt old-set) new-set elt))))

(defun first-common-member (a b)
  (loop for x in a do
       (if (member x b) 
	   (return t))))

(defun acceptable-continuation (initial current right-walls)
  "t if current set is okay given initial set."
  ;; we want at least one set mentioned in the initial to appear in the current
  (and (first-common-member initial current)
       
;;))
;;(defun xxx ()
       ;; we also want at there at least 50% different sets
       ;; adjacent cells without a wall count as 1
       (let ((num 0) (sets nil))
	 (loop for x in current 
	    for wall in right-walls do
	      (and wall (incf num)
		   (not (member x sets))
		   (setq sets (cons x sets))))
	 (or (> (length sets) 4)
	      (> (length sets) (* 0.5 num))))))

      
(defun compress-node (n &optional exclude-nodes)
  (unless (member n exclude-nodes)
    (if (> (length n) 3)
      (loop for n2 in (cddr n) do
	   (compress-node n2 (cons n exclude-nodes)))
      (when (= (length n) 3)
	;; new ID
	 (setf (car n) (intern (concatenate 'string (symbol-name (car n)) "-"  (symbol-name (car (third n))))))
	;; add up length
	(setf (second n) (+ (second n) (second (third n))))
	;; and get rid of second node
	(setf (cddr n) (cddr (third n)))
	;; compress rest
	(compress-node n exclude-nodes)))))

(defun compress-one-node (n)
    (when (= (length n) 3)
	;; new ID
	 (setf (car n) 
	       (intern (concatenate 'string (symbol-name (car n)) "-"  (symbol-name (car (third n))))))
	;; add up length
	(setf (second n) (+ (second n) (second (third n))))
	;; and get rid of second node
	(setf (cddr n) (cddr (third n)))))


 
(make-random-state t)


(defun the-shortest-path (maze)
  (setq *shortest-paths* nil)
  (let* ((from  (car (third (first maze))))
	 (to (car (third (second maze))))
	 (fm (flatten-maze maze)))
     (or (assoc to (shortest-paths from fm))
)))
;	(assoc from (shortest-paths to fm)))))


(defun flatten-maze (maze)
  (reverse (flat-graph (third maze) nil)))


(defun flat-graph (recursive-graph graph-node-alist)
  (let ((name (first recursive-graph))
	(nodes (cddr recursive-graph)))
    
    (unless (eq name 'START)
      (push (cons name
		  (mapcar #'graph-name nodes))
	    graph-node-alist))

    (mapc
     (lambda (n)
       (unless (assoc (graph-name n) graph-node-alist)
	 (setq graph-node-alist (flat-graph n graph-node-alist))))
     nodes))
  graph-node-alist)
    


(defun graph-name (g)
  (first g))

(defvar *shortest-paths* nil)

(defun shortest-paths (j &optional (graph *graph*) (trace nil))
  "Computes shortest path from j too all chunks in graph."
  (or (rest (assoc j *shortest-paths*))
      (let ((paths (list (list j 0 (list j))))
            (hull (list j))
            (length 0))
        (loop
         (when trace (format t "HULL ~S~%" hull))
         (incf length)
         (let ((next-hull nil))
           (dolist (location hull)
             (when trace (format t "LOCATION ~S~%" location))
             (let ((reachable (rest (assoc location graph))))
               (dolist (next-location reachable)
                 (when trace (format t "NEXT ~S~%" next-location))
                 (let ((current-path (assoc next-location paths)))
                   (unless current-path ;;; if no current path to that location, add it
                     (setf current-path (list next-location length))
                     (push current-path paths)
                     (push next-location next-hull))
                   (when (= length (second current-path)) ;;; if longer way to reach next-location, then forget about it
                     (let ((location-paths (cddr (assoc location paths))))
                       (dolist (location-path location-paths)
                         (when trace (format t "PATH ~S~%" location-path))
                         (nconc current-path (list (append location-path (list next-location)))))))))))
           (when trace (format t "PATHS ~S~%" paths))
           (if next-hull
               (setf hull next-hull)
             (return))))
        ;;; cache in *shortest-paths*
        (setf paths (reverse paths))
        (push (cons j paths) *shortest-paths*)
        paths)))

(defvar *maze* nil)
(defun demo ()
  (let ((*graphical-solve* nil))
    (setq *maze* (make-maze 10 10))

    (the-shortest-path *maze*)))





;; experiment system

(defvar *maze-size* nil)
(defvar *output* "")
(defun run-trial (n &optional (condition-code ""))
  (let ((solved nil)
	(time nil)
	(solution-length nil)
	(bt-solution-length nil)
	(min-solution-length nil))
    
    (loop for trial from 1 to n do
	 (make-random-state t)
	 (let ((*temp-file*
		(format nil "/tmp/maze-~a-~s.png" condition-code trial))
	       (*temp-file-gif*
		(format nil "/tmp/maze-~a-~s.gif" condition-code trial)))
	   (let* ((mm (make-maze *maze-size* *maze-size* 40))
		  (sp (the-shortest-path mm)))
	     
	   (push  (second sp) min-solution-length)
	   (push *solution-found* solved)
	   (push *sim-time* time)
	   (push (length *solution-squares*) solution-length)
	   
	   ;; find alternative (backtracking-based) solution
	   
	   (solve-maze-cognitively mm)
	   (push (length *solution-squares*) bt-solution-length)
	   (setq *output* 
		 (concatenate 'string *output*
			      (format nil "~a ~s ~s ~s ~s ~s ~%" condition-code (car solved) (car time) (car solution-length) (car bt-solution-length) (car min-solution-length))))))))
  ;; (print (format nil "Solved: ~s\nTime:~s\nSolution Length: ~s\nBT Solution: ~s\n" solved time solution-length bt-solution-length)))
  nil)



(defun run-experiment-1 ()

  
  (setq *output* "")

  (let ((*display-results* t)) 
    (loop for *maze-size* in (list 10 15 20 25 30) do
	 (loop for *visual-num-finsts* from 2 to 40 do
	      
	      (run-trial 10
					; condition-code
			 (format nil "~s ~s " *maze-size* *visual-num-finsts*))
	      ))
    (print  (format nil "size finsts solved time sol.len bt.sol.len min.sol.len ~%"))
    (print *output*)
    nil))


;; interface to ACT-R model

(defvar *integr-model-maze-image* nil)

(defvar *image* nil)

(defvar *visual-count* 0)
(defvar *walk-count* 0)



(defun run-integrated-model (&optional maze)
"
Make a maze and run integrated model, or reuse maze.
Model is run for each of all possible start/end points.
"
 (setq *solution-squares* nil)

 
   (let* ((*graphical-solve* nil)
	  (graph (or maze (make-maze)))
	  (from (third (first graph)))
	  (to (third (second graph)))
	  (cells-h 20) (cells-v 20)
	  (cell-width 20) (cell-height 20) (*square-width* 20)
	  (*robot-width* 2)
	  (canvas-width (* cell-width (+ 2 cells-h)))
	  (canvas-height (* cell-height (+ 2 cells-v))))

    (vecto::with-canvas (:width canvas-width :height canvas-height)
      (vecto::with-graphics-state

       (print canvas-width)
    (setq *solution-found* 'invalid)
    
    (let* ((wall-ratio *wall-ratio*)	;; out of 10
	   (v-wall-ratio *wall-ratio*)
	   (start-end-top-imagefile nil))

   
      (setq start-end-top-imagefile (draw-new-maze *square-width* cells-h cells-v wall-ratio v-wall-ratio))

      (reset-model-chunks) ;; ACT-R call

      (loop for trial from 1 to 1 do
	   (setq *display-results* (member trial '(1 2 6 10)))
      (loop for challenge in (fifth start-end-top-imagefile) do
	   
	   (init-cog-model) ;; clear visual trace

	   
	   (let* (;; (canvas-width (skippy::width *image*)) ; prepare vecto image with the "track"
		  ;; 	     (canvas-height (skippy::height *image*))
		  ;; 	     (canvas (skippy::make-canvas :width canvas-width :height canvas-height
		  ;; 					  :image-data (skippy::image-data *image*)))
		  (*visual-count* 0) (*walk-count* 0))
	     
	     (find-waypoints-draw-to-current-canvas 
	    ; for each matched start-end point  (third (car challenge)) (third (cdr challenge)) 
	      ; for single end point, but changing start points:
	      (third (car challenge)) (third (cdr (first (fifth start-end-top-imagefile))))
	      (aref (skippy::images (skippy::load-data-stream (fourth start-end-top-imagefile))) 0))  ; reload the created image file
	     )))
      (format t "saving to ~s" *temp-file*)
      (vecto::save-png *temp-file*)
      
      (when (eq *display-results* 'show)
	(display-image *temp-file*))
)))))


(defun run-integrated-model-on-image-file (file start end)
  "
How are we running the integrated model when there are no squares?

We need to allow for arbirtrary pixel coordinates, right?

We may need to set *density-sampling* to t, but then the colors have to be right.
"

  (let ((image (aref (skippy::images (skippy::load-data-stream (convert-to-gif file))) 0)))
    

    (setq *solution-squares* nil)
    
    
    (let* ((*graphical-solve* nil)
	   (graph (or maze (make-maze)))
	   (from (third (first graph)))
	   (to (third (second graph)))
	   (cells-h 20) (cells-v 20)
	   (cell-width 10) (cell-height 10) (*square-width* 10)
	   (canvas-width (* cell-width (+ 2 cells-h)))
	   (canvas-height (* cell-height (+ 2 cells-v))))

      (find-waypoints-draw-to-current-canvas start end image)
      )))

(defun find-waypoints-draw-to-current-canvas (start-node end-node image)
 
  (let ((*image* image))
    ;; start with direct line
    (vecto::set-rgb-stroke 0 1.0 0)
    (vecto::set-line-width 2)

    ;; run model

    (setq *graph* nil) ;; no graph initially
    (reset-model-parameters nil)


    (let ((*navigation-mode* 'memory)
	  (*verbose* t))
      (walk-to (apply #'square-point (name-location (graph-name start-node))))
      
      (plan (graph-name start-node) (graph-name end-node) 18.0 nil)
      ;; callback is done by plan to visual-search
      )
    (print (format nil "walk: ~s  visual: ~s" *walk-count* *visual-count*))
    (when (and *display-results*  ;; (> *walk-count* (+ 1 *visual-count* ))
	       ) ; if more than one memory step
      (format t "visualizing ~a" *visual-trace-path*)

      (visualize-path *visual-trace-path* nil nil *visual-trace-path-meta*)
      )
    ))

(defun visual-search (current-chunk target-chunk)
  (format t "vis search.  current location: ~a   current-chunk: ~a"
	  *visual-attention-loc* current-chunk)

  (incf *visual-count*)
  (let ((*navigation-mode* 'visual) 
	(cur-loc (name-location current-chunk))
	(target-loc (name-location target-chunk)))
    (move-to-next-target *image* (apply #'square-point target-loc)))
;  (mark-point *visual-attention-loc* *blue*)
;; maybe this doesn't return the right chunk or so.
;; or the visual loc isn't tracked correctly.
  (location-name (point-square  *visual-attention-loc*)))

(defun visual-move-to (target-chunk)
  (incf *walk-count*)
  (print 'walk-to)
  (print target-chunk)
  (walk-to (apply #'square-point (name-location target-chunk)))
; (when *display-results*
    (mark-point *visual-attention-loc* *green*))


(defun read-geo-data (path id time)

	 (let ((fp (open (format nil "~a~a_~a.tfw" path id time) :direction :input))
	       pixel-width top left)
	   (setq pixel-width (read-from-string (read-line fp)))
	   (read-from-string (read-line fp))
	   (read-from-string (read-line fp))
	   (read-from-string (read-line fp))
 	   (setq left (read-from-string (read-line fp))
		 top (read-from-string (read-line fp)))
	   (close fp)

	   (list pixel-width left top)))

;(asdf:oos 'asdf:load-op 'cl-ppcre)
(ql:quickload "cl-ppcre")

(defun run-over-geo-files ()
  
  (let* ((dirpath "./path-data/part2/*Mid.log") ;"/Users/dr/path-data/part2/*Mid.log")
	 (stats-output (open "./stats-output/model-results.txt" ;"/Users/dr/MURI/path/part1/model-results.txt" 
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


    