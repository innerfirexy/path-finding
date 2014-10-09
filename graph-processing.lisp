#|
Graph as list of nodes, each of which is id plus list of other ids (or nodes)?
e.g.
(setf *graph* '((a b) (b a c d) (c b d e) (d b c e) (e c d)))

12/29/08 Make general graphs such as trees and grids
correct IA by base-level for true log likelihood

1/2/09 Correct error of 0/0 for non-connected graphs in setting base-levels and associations

1/5/09 BLLs and IAs for paths instead of locations

1/6/09 Make AIs symmetrical to prevent inhibition from destination
Reset paths state and clear old plans
Set similarities between locations
Initially: based on least distance, which is a bit cheating
Exponential rather than linear function seems to work better

|#

(defvar *graph* nil)

(defvar *paths* nil)

(defun make-chunks (&optional (graph *graph*))
  "Creates location and paths chunks to encode graph."
  (let ((chunks nil))
    (setf *paths* nil)
    (dolist (node graph)
      (let ((chunk (first node)))
        (push (list chunk 'isa 'location) chunks)
        (dolist (target (rest node))
          (let ((path (intern (format nil "~S-TO-~S" chunk target))))
            (push (list path chunk target) *paths*)
            (push (list path 'isa 'path 'current chunk 'target target 'state 'free) chunks)))))
    (add-dm-fct (nreverse chunks))))

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

;;; Set association from location j to path i as log sum over all locations k of the probability that path i is on the shortest path from j to k

(defun set-location-associations (&optional (graph *graph*) (paths *paths*) (trace nil))
  (let ((associations nil)
        (nodes (length graph)))
    (dolist (j graph)
      (dolist (i paths)
        (let ((association 0.0)
              (log-base-level (no-output (- (caar (sdp-fct (list i :base-level))) (car (sgp :blc))))))
          (dolist (k graph (setf association (- (log (/ (max association (/ 1.0 nodes)) (* 2.0 nodes))) log-base-level)))  ;;; FIX: both ways
            (let ((j-k-paths (cddr (assoc (first k) (shortest-paths (first j) graph)))) ;;; specify graph
                  (p 0.0))
              (when j-k-paths
                (dolist (j-k-path j-k-paths)
                  (when (search (rest i) j-k-path) (incf p))) ;;; FIX: (rest i) is list of (current target)
                (setf p (/ p (length j-k-paths))))
              (when trace (format t "Prob that ~S is on path from ~S to ~S is ~6,3F~%" (first i) (first j) (first k) p))
              (incf association p))
            ;;; FIX: Also do reverse because primed from both ends
            (let ((k-j-paths (cddr (assoc (first j) (shortest-paths (first k) graph)))) ;;; specify graph
                  (p 0.0))
              (when k-j-paths
                (dolist (k-j-path k-j-paths)
                  (when (search (rest i) k-j-path) (incf p))) ;;; FIX: (rest i) is list of (current target)
                (setf p (/ p (length k-j-paths))))
              (when trace (format t "Prob that ~S is on path from ~S to ~S is ~6,3F~%" (first i) (first k) (first j) p))
              (incf association p))
            )
          (when trace (format t "Association from chunk ~S to chunk ~S is ~6,3F~%" (first j) (first i) association))
          (push (list (first j) (first i) association) associations))))
    (setf associations (reverse associations))
    (set-ia-fct associations)
    associations))

;;; Set base-level of path i as log sum over all locations j and k of the probability that path i is on shortest path from j to k

(defun set-location-baselevels (&optional (graph *graph*) (paths *paths*) (trace nil))
  (let ((baselevels nil)
        (nodes (length graph)))
    (dolist (i paths)
      (let ((base-level 0.0))
        (dolist (j graph)
          (dolist (k graph)
            (let ((j-k-paths (cddr (assoc (first k) (shortest-paths (first j) graph)))) ;;; specify graph
                   (p 0.0))
              (when j-k-paths
                (dolist (j-k-path j-k-paths)
                  (when (search (rest i) j-k-path) (incf p))) ;;; FIX: (rest i) is list of (current target)
                (setf p (/ p (length j-k-paths))))
              (when trace (format t "Prob that ~S is on path from ~S to ~S is ~6,3F~%" (first i) (first j) (first k) p))
              (incf base-level p))))
        (setf base-level (+ (log (/ base-level (* nodes nodes))) (no-output (first (sgp :blc))))) ;;; add :BLC
        (when trace (format t "Base-level of chunk ~S is ~6,3F including BLC~%" (first i) base-level))
        (push (list (first i) base-level) baselevels)))
    (setf baselevels (reverse baselevels))
    (set-base-levels-fct baselevels)
    baselevels))

;;; Set distance between locations based on graph distance

(defvar *similarity-factor* 0.1) ;;; linear factor of 0.1 unit of activation per distance - could normalize by graph size

(defun set-location-similarities (&optional (graph *graph*) (trace nil))
  (let ((similarities nil)) 
    (dolist (i graph)
      (dolist (j graph)
        (let ((distance-i-j (or (second (assoc (first j) (shortest-paths (first i) graph)))
                            *max-dif*)))
;;;          (push (list (first i) (first j) (- *max-sim* (* *similarity-factor* distance-i-j))) similarities)
          (push (list (first i) (first j) (- (exp (- distance-i-j)) 1.0)) similarities)
          (when trace (format t "Similarity between ~S and ~S is ~6,3F~%" (first i) (first j) (third (first similarities)))))))
    (setf similarities (reverse similarities))
    (set-similarities-fct similarities)
    similarities))

;;; Model running

(defvar *path* nil)

(defun move (location)
  (push location *path*))

(defun reset-model-parameters (&optional (graph *graph*))
  (unless (eq graph *graph*)
    (setf *graph* graph)
    (setf *shortest-paths* nil))
  (reset)
  (make-chunks)
  (set-location-baselevels)
  (set-location-associations)
  (set-location-similarities)
  (length (no-output (sdm isa location))))

(defun reset-model-chunks ()
  "Clears old plans and reset paths to free."
  (no-output
   (delete-chunk-fct (sdm isa plan))
   (dolist (path (sdm isa path state used))
     (mod-chunk-fct path '(state free)))))

(defun plan (a b &optional (time nil) (reset t))
  (setf *path* (list a))
  (when reset (reset-model-chunks))
  (goal-focus-fct (add-dm-fct (list (list (gentemp "GOAL") 'isa 'plan
                                          'current a 'target b
                                          'state 'planning
                                          'parent 'none)) :reset-ia nil))
  (if time (run-fct time) (run))
  (reverse *path*))

(defun make-tree (deep wide &optional (parent 'node) (grandparent nil))
  "Generate graph for tree of depth deep and branching factor wide."
  (let ((connections (list parent))
        (graph nil))
    (when grandparent (push grandparent connections))
    (unless (zerop deep)
      (dotimes (branch wide)
        (let ((node (intern (format nil "~A-~D" parent (1+ branch)))))
          (push node connections)
          (setf graph (nconc graph (make-tree (1- deep) wide node parent))))))
    (setf connections (nreverse connections))
    (setf graph (cons connections graph))))

(defun make-grid (deep wide)
  "Generate grid for graph of depth deep and width wide."
  (let ((grid (make-array (list deep wide) :initial-element nil))
        (graph nil))
    (dotimes (depth deep)
      (dotimes (width wide)
        (let ((node (intern (format nil "GRID-~D-~D" (1+ depth) (1+ width)))))
          (setf (aref grid depth width) node))))
    (dotimes (depth deep)
      (dotimes (width wide)
        (let ((connections (list (aref grid depth width))))
          (unless (zerop depth)
            (push (aref grid (1- depth) width) connections))
          (unless (zerop width)
            (push (aref grid depth (1- width)) connections))
          (unless (= depth (1- deep))
            (push (aref grid (1+ depth) width) connections))
          (unless (= width (1- wide))
            (push (aref grid depth (1+ width)) connections))
          (setf connections (nreverse connections))
          (push connections graph))))
    (setf graph (nreverse graph))))

(defun random-pareto (&optional (xm 1) (k 1))
   "Returns a random sample from a pareto distribution.  We're using inverse transform sampling to obtain the sample."
   (/ xm (expt (random 1.0) (/ 1 k))))

(defvar *results* nil)

(defun get-graph-stats (&optional (graph *graph*) (noises '(nil 0.1 0.25 0.5 0.75 1.0)) (samples 10) (time-limit 60.0) (trace t))
  "Run the model through every pair of graph destinations."
  (let ((results nil))
    (dolist (i graph)
      (dolist (j graph)
        (dolist (ans noises)
          (reset-model-parameters graph)
          (sgp-fct (list :ans ans))
          (dotimes (sample samples)
            (let* ((shortest (second (assoc (first j) (shortest-paths (first i) graph))))
                   (path (plan (first i) (first j) time-limit))
                   (length (1- (length path)))
                   (success (and (eq (first path) (first i)) (eq (first (last path)) (first j))))
                   (perfect (and success (= length shortest)))
                   (result (assoc (cons shortest ans) results :test #'equal)))
              (unless result 
                (setf result (list (cons shortest ans) 0 0 0 0))
                (push result results))
              (incf (second result))
              (when success
                (incf (third result))
                (when perfect (incf (fourth result)))
                (incf (fifth result) length)))))))
    (setf results (reverse results))
    (when trace (format t "~S~%" results))
    (push results *results*)
    results))

(defun aggregate-by-noise (results-list)
  (let ((average nil))
    (dolist (results results-list)
      (dolist (result results)
        (let* ((noise (or (cdr (first result)) 0.0))
               (record (assoc noise average :test #'equal)))
          (unless record
            (setf record (list noise 0 0 0 0))
            (push record average))
          (incf (second record) (second result))
          (incf (third record) (third result))
          (incf (fourth record) (fourth result))
          (incf (fifth record) (if (zerop (caar result)) (third result)
                                 (/ (fifth result) (caar result)))))))
    (setf average (sort average #'< :key #'car))
    (dolist (record average)
      (format t "~6,2F~C~6,3F~C~6,3F~C~6,3F~%" (first record) #\tab
              (/ (third record) (second record)) #\tab
              (/ (fourth record) (third record)) #\tab
              (/ (fifth record) (third record))))))
              
(defun aggregate-by-length (results-list)
  (let ((average nil))
    (dolist (results results-list)
      (dolist (result results)
        (let* ((length (or (car (first result)) 0))
               (record (assoc length average :test #'equal)))
          (unless record
            (setf record (list length 0 0 0 0))
            (push record average))
          (incf (second record) (second result))
          (incf (third record) (third result))
          (incf (fourth record) (fourth result))
          (incf (fifth record) (if (zerop length) (third result)
                                 (/ (fifth result) length))))))
    (setf average (sort average #'< :key #'car))
    (dolist (record average)
      (format t "~6,2F~C~6,3F~C~6,3F~C~6,3F~%" (first record) #\tab
              (/ (third record) (second record)) #\tab
              (/ (fourth record) (third record)) #\tab
              (/ (fifth record) (third record))))))
              

#|
CL-USER 1199 > (time
                (progn
                  (get-graph-stats (make-tree 2 2))
                  (get-graph-stats (make-grid 2 2))
                  (get-graph-stats (make-tree 2 3))
                  (get-graph-stats (make-grid 3 3))
                  (get-graph-stats (make-tree 3 2))
                  (get-graph-stats (make-grid 4 4))
                  (get-graph-stats (make-tree 3 3))
                  (get-graph-stats (make-grid 5 5))
                  (get-graph-stats (make-tree 3 4))
                  (get-graph-stats (make-grid 6 6))
                  (get-graph-stats (make-tree 4 3))
                  (get-graph-stats (make-grid 7 7))
                  (get-graph-stats (make-tree 4 4))
                  (get-graph-stats (make-grid 8 8))
                  (get-graph-stats (make-tree 4 5))
                  (get-graph-stats (make-grid 9 9))
                  (get-graph-stats (make-tree 5 4))
                  (get-graph-stats (make-grid 10 10))))
Timing the evaluation of (PROGN (GET-GRAPH-STATS (MAKE-TREE 2 2)) (GET-GRAPH-STATS (MAKE-GRID 2 2)) (GET-GRAPH-STATS (MAKE-TREE 2 3)) (GET-GRAPH-STATS (MAKE-GRID 3 3)) (GET-GRAPH-STATS (MAKE-TREE 3 2)) (GET-GRAPH-STATS (MAKE-GRID 4 4)) (GET-GRAPH-STATS (MAKE-TREE 3 3)) (GET-GRAPH-STATS (MAKE-GRID 5 5)) (GET-GRAPH-STATS (MAKE-TREE 3 4)) (GET-GRAPH-STATS (MAKE-GRID 6 6)) (GET-GRAPH-STATS (MAKE-TREE 4 3)) (GET-GRAPH-STATS (MAKE-GRID 7 7)) (GET-GRAPH-STATS (MAKE-TREE 4 4)) (GET-GRAPH-STATS (MAKE-GRID 8 8)) (GET-GRAPH-STATS (MAKE-TREE 4 5)) (GET-GRAPH-STATS (MAKE-GRID 9 9)) (GET-GRAPH-STATS (MAKE-TREE 5 4)) (GET-GRAPH-STATS (MAKE-GRID 10 10)))
(((0) 70 70 70 0) ((0 . 0.1) 70 70 70 0) ((0 . 0.25) 70 70 70 0) ((0 . 0.5) 70 70 70 0) ((0 . 0.75) 70 70 70 0) ((0 . 1.0) 70 70 70 0) ((1) 120 120 120 120) ((1 . 0.1) 120 120 120 120) ((1 . 0.25) 120 120 120 120) ((1 . 0.5) 120 120 114 132) ((1 . 0.75) 120 116 91 176) ((1 . 1.0) 120 104 78 182) ((2) 140 140 140 280) ((2 . 0.1) 140 140 140 280) ((2 . 0.25) 140 139 137 282) ((2 . 0.5) 140 131 121 282) ((2 . 0.75) 140 117 98 284) ((2 . 1.0) 140 98 67 286) ((3) 80 80 80 240) ((3 . 0.1) 80 80 80 240) ((3 . 0.25) 80 79 79 237) ((3 . 0.5) 80 72 71 218) ((3 . 0.75) 80 48 45 150) ((3 . 1.0) 80 45 35 155) ((4) 80 80 80 320) ((4 . 0.1) 80 80 80 320) ((4 . 0.25) 80 80 80 320) ((4 . 0.5) 80 60 59 242) ((4 . 0.75) 80 34 31 142) ((4 . 1.0) 80 30 22 136))
(((0) 40 40 40 0) ((0 . 0.1) 40 40 40 0) ((0 . 0.25) 40 40 40 0) ((0 . 0.5) 40 40 40 0) ((0 . 0.75) 40 40 40 0) ((0 . 1.0) 40 40 40 0) ((1) 80 80 80 80) ((1 . 0.1) 80 80 80 80) ((1 . 0.25) 80 80 80 80) ((1 . 0.5) 80 80 80 80) ((1 . 0.75) 80 76 71 92) ((1 . 1.0) 80 76 65 108) ((2) 40 40 40 80) ((2 . 0.1) 40 40 40 80) ((2 . 0.25) 40 40 40 80) ((2 . 0.5) 40 40 38 86) ((2 . 0.75) 40 38 34 86) ((2 . 1.0) 40 38 33 92))
(((0) 130 130 130 0) ((0 . 0.1) 130 130 130 0) ((0 . 0.25) 130 130 130 0) ((0 . 0.5) 130 130 130 0) ((0 . 0.75) 130 130 130 0) ((0 . 1.0) 130 130 130 0) ((1) 240 240 240 240) ((1 . 0.1) 240 240 240 240) ((1 . 0.25) 240 240 236 248) ((1 . 0.5) 240 238 209 298) ((1 . 0.75) 240 228 175 356) ((1 . 1.0) 240 213 144 431) ((2) 420 420 420 840) ((2 . 0.1) 420 420 420 840) ((2 . 0.25) 420 420 414 852) ((2 . 0.5) 420 388 331 894) ((2 . 0.75) 420 350 256 944) ((2 . 1.0) 420 271 155 942) ((3) 360 360 360 1080) ((3 . 0.1) 360 360 360 1080) ((3 . 0.25) 360 355 355 1065) ((3 . 0.5) 360 317 306 973) ((3 . 0.75) 360 234 192 796) ((3 . 1.0) 360 162 116 624) ((4) 540 540 540 2160) ((4 . 0.1) 540 540 540 2160) ((4 . 0.25) 540 535 535 2140) ((4 . 0.5) 540 415 387 1716) ((4 . 0.75) 540 284 227 1280) ((4 . 1.0) 540 149 95 744))
(((0) 90 90 90 0) ((0 . 0.1) 90 90 90 0) ((0 . 0.25) 90 90 90 0) ((0 . 0.5) 90 90 90 0) ((0 . 0.75) 90 90 90 0) ((0 . 1.0) 90 90 90 0) ((1) 240 240 240 240) ((1 . 0.1) 240 240 240 240) ((1 . 0.25) 240 240 240 240) ((1 . 0.5) 240 240 228 278) ((1 . 0.75) 240 234 191 396) ((1 . 1.0) 240 219 153 491) ((2) 280 280 280 560) ((2 . 0.1) 280 280 280 560) ((2 . 0.25) 280 280 279 562) ((2 . 0.5) 280 279 253 636) ((2 . 0.75) 280 257 191 728) ((2 . 1.0) 280 222 124 856) ((3) 160 160 160 480) ((3 . 0.1) 160 160 160 480) ((3 . 0.25) 160 160 159 482) ((3 . 0.5) 160 154 141 512) ((3 . 0.75) 160 146 88 664) ((3 . 1.0) 160 117 56 633) ((4) 40 40 40 160) ((4 . 0.1) 40 40 40 160) ((4 . 0.25) 40 40 40 160) ((4 . 0.5) 40 37 31 176) ((4 . 0.75) 40 33 21 210) ((4 . 1.0) 40 32 14 212))
(((0) 150 150 150 0) ((0 . 0.1) 150 150 150 0) ((0 . 0.25) 150 150 150 0) ((0 . 0.5) 150 150 150 0) ((0 . 0.75) 150 150 150 0) ((0 . 1.0) 150 150 150 0) ((1) 280 280 280 280) ((1 . 0.1) 280 280 279 282) ((1 . 0.25) 280 280 268 308) ((1 . 0.5) 280 278 218 420) ((1 . 0.75) 280 268 192 486) ((1 . 1.0) 280 240 153 546) ((2) 380 380 380 760) ((2 . 0.1) 380 380 380 760) ((2 . 0.25) 380 372 351 788) ((2 . 0.5) 380 341 260 876) ((2 . 0.75) 380 299 200 892) ((2 . 1.0) 380 218 131 748) ((3) 400 400 400 1200) ((3 . 0.1) 400 400 399 1202) ((3 . 0.25) 400 386 368 1194) ((3 . 0.5) 400 331 276 1135) ((3 . 0.75) 400 229 159 871) ((3 . 1.0) 400 139 92 565) ((4) 400 400 400 1600) ((4 . 0.1) 400 399 399 1596) ((4 . 0.25) 400 381 376 1534) ((4 . 0.5) 400 285 246 1222) ((4 . 0.75) 400 181 126 898) ((4 . 1.0) 400 91 59 454) ((5) 320 320 320 1600) ((5 . 0.1) 320 320 320 1600) ((5 . 0.25) 320 302 301 1512) ((5 . 0.5) 320 203 196 1029) ((5 . 0.75) 320 101 81 553) ((5 . 1.0) 320 50 31 302) ((6) 320 320 320 1920) ((6 . 0.1) 320 316 316 1896) ((6 . 0.25) 320 306 304 1840) ((6 . 0.5) 320 192 185 1166) ((6 . 0.75) 320 63 53 400) ((6 . 1.0) 320 30 20 202))
(((0) 160 160 160 0) ((0 . 0.1) 160 160 160 0) ((0 . 0.25) 160 160 160 0) ((0 . 0.5) 160 160 160 0) ((0 . 0.75) 160 160 160 0) ((0 . 1.0) 160 160 160 0) ((1) 480 480 480 480) ((1 . 0.1) 480 480 480 480) ((1 . 0.25) 480 480 480 480) ((1 . 0.5) 480 479 449 555) ((1 . 0.75) 480 471 368 825) ((1 . 1.0) 480 416 275 1250) ((2) 680 680 680 1360) ((2 . 0.1) 680 680 677 1366) ((2 . 0.25) 680 680 667 1390) ((2 . 0.5) 680 670 572 1700) ((2 . 0.75) 680 628 384 2330) ((2 . 1.0) 680 517 263 2406) ((3) 640 640 640 1920) ((3 . 0.1) 640 640 638 1928) ((3 . 0.25) 640 639 604 2005) ((3 . 0.5) 640 618 473 2350) ((3 . 0.75) 640 548 283 2968) ((3 . 1.0) 640 391 143 2653) ((4) 400 400 400 1600) ((4 . 0.1) 400 400 400 1600) ((4 . 0.25) 400 396 376 1626) ((4 . 0.5) 400 378 260 1894) ((4 . 0.75) 400 328 141 2320) ((4 . 1.0) 400 213 67 1732) ((5) 160 160 160 800) ((5 . 0.1) 160 160 160 800) ((5 . 0.25) 160 158 152 804) ((5 . 0.5) 160 144 93 898) ((5 . 0.75) 160 116 39 950) ((5 . 1.0) 160 81 23 825) ((6) 40 40 40 240) ((6 . 0.1) 40 40 40 240) ((6 . 0.25) 40 40 38 244) ((6 . 0.5) 40 36 26 258) ((6 . 0.75) 40 25 11 218) ((6 . 1.0) 40 15 3 178))
(((0) 400 400 400 0) ((0 . 0.1) 400 400 400 0) ((0 . 0.25) 400 400 400 0) ((0 . 0.5) 400 400 400 0) ((0 . 0.75) 400 400 400 0) ((0 . 1.0) 400 400 400 0) ((1) 780 780 240 1860) ((1 . 0.1) 780 780 475 1534) ((1 . 0.25) 780 779 497 1499) ((1 . 0.5) 780 774 443 1670) ((1 . 0.75) 780 727 418 1771) ((1 . 1.0) 780 627 349 1679) ((2) 1500 960 960 1920) ((2 . 0.1) 1500 1300 833 3680) ((2 . 0.25) 1500 1312 808 3848) ((2 . 0.5) 1500 1245 732 3888) ((2 . 0.75) 1500 1100 544 3920) ((2 . 1.0) 1500 816 378 3472) ((3) 1980 1980 1980 5940) ((3 . 0.1) 1980 1750 1632 5486) ((3 . 0.25) 1980 1721 1553 5509) ((3 . 0.5) 1980 1505 1134 5365) ((3 . 0.75) 1980 1109 667 4619) ((3 . 1.0) 1980 711 377 3319) ((4) 3240 3240 3240 12960) ((4 . 0.1) 3240 2925 2777 11996) ((4 . 0.25) 3240 2854 2621 11890) ((4 . 0.5) 3240 2220 1695 10156) ((4 . 0.75) 3240 1452 891 7416) ((4 . 1.0) 3240 726 383 4196) ((5) 3240 3240 3240 16200) ((5 . 0.1) 3240 3067 3067 15335) ((5 . 0.25) 3240 2994 2973 15012) ((5 . 0.5) 3240 2119 1940 10971) ((5 . 0.75) 3240 1144 845 6460) ((5 . 1.0) 3240 449 281 2743) ((6) 4860 4860 4860 29160) ((6 . 0.1) 4860 4590 4590 27540) ((6 . 0.25) 4860 4441 4424 26680) ((6 . 0.5) 4860 2698 2467 16664) ((6 . 0.75) 4860 1169 862 7814) ((6 . 1.0) 4860 451 284 3230))
(((0) 250 250 250 0) ((0 . 0.1) 250 250 250 0) ((0 . 0.25) 250 250 250 0) ((0 . 0.5) 250 250 250 0) ((0 . 0.75) 250 250 250 0) ((0 . 1.0) 250 250 250 0) ((1) 800 800 800 800) ((1 . 0.1) 800 800 800 800) ((1 . 0.25) 800 800 798 804) ((1 . 0.5) 800 799 739 959) ((1 . 0.75) 800 767 579 1577) ((1 . 1.0) 800 685 458 1937) ((2) 1240 1240 1240 2480) ((2 . 0.1) 1240 1240 1213 2546) ((2 . 0.25) 1240 1238 1169 2652) ((2 . 0.5) 1240 1229 977 3308) ((2 . 0.75) 1240 1145 657 4764) ((2 . 1.0) 1240 846 397 4946) ((3) 1360 1360 1360 4080) ((3 . 0.1) 1360 1343 1288 4207) ((3 . 0.25) 1360 1356 1215 4480) ((3 . 0.5) 1360 1320 918 5566) ((3 . 0.75) 1360 1156 515 7118) ((3 . 1.0) 1360 743 267 5629) ((4) 1200 1200 1200 4800) ((4 . 0.1) 1200 1187 1129 4966) ((4 . 0.25) 1200 1186 1023 5268) ((4 . 0.5) 1200 1119 695 6232) ((4 . 0.75) 1200 911 336 7306) ((4 . 1.0) 1200 487 148 4484) ((5) 800 800 800 4000) ((5 . 0.1) 800 791 749 4119) ((5 . 0.25) 800 785 678 4255) ((5 . 0.5) 800 705 370 5069) ((5 . 0.75) 800 564 178 5290) ((5 . 1.0) 800 279 60 3201) ((6) 400 400 400 2400) ((6 . 0.1) 400 400 384 2450) ((6 . 0.25) 400 390 334 2498) ((6 . 0.5) 400 350 178 2934) ((6 . 0.75) 400 262 79 2776) ((6 . 1.0) 400 112 23 1360) ((7) 160 160 160 1120) ((7 . 0.1) 160 159 157 1119) ((7 . 0.25) 160 156 140 1136) ((7 . 0.5) 160 138 62 1318) ((7 . 0.75) 160 94 20 1296) ((7 . 1.0) 160 39 4 635) ((8) 40 40 40 320) ((8 . 0.1) 40 40 39 324) ((8 . 0.25) 40 35 32 290) ((8 . 0.5) 40 29 18 286) ((8 . 0.75) 40 26 1 438) ((8 . 1.0) 40 10 0 152))

Break.
  1 (continue) Return from break.
  2 (abort) Return to level 0.
  3 Return to top loop level 0.

Type :b for backtrace, :c <option number> to proceed,  or :? for other options
                  

CL-USER 1200 : 1 > 
|#

#|
CL-USER 454 > (reset-model-parameters (make-tree 'node 4 2))
31

CL-USER 455 > (plan 'node-1-1-1 'node-1-2-2)
 Time  0.000: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-1-2-2 
 Time  0.050: Check-Direct-Link Fired
 Time  1.050: Failure Retrieved
 Time  1.050: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-1-2-2 - SUBGOALING 
 Time  1.100: No-Direct-Link Fired
 Time  1.100: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Node-1-1-1 AND Node-1-2-2 
 Time  1.150: Find-Intermediate-Location Fired
 Time  1.155: Node-1 Retrieved
 Time  1.155: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Node-1 
 Time  1.205: Subgoal-Intermdiate-Location Fired
 Time  1.205: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-1 
 Time  1.255: Check-Direct-Link Fired
 Time  2.255: Failure Retrieved
 Time  2.255: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-1 - SUBGOALING 
 Time  2.305: No-Direct-Link Fired
 Time  2.305: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Node-1-1-1 AND Node-1 
 Time  2.355: Find-Intermediate-Location Fired
 Time  2.366: Node-1-1 Retrieved
 Time  2.366: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Node-1-1 
 Time  2.416: Subgoal-Intermdiate-Location Fired
 Time  2.416: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-1-1 
 Time  2.466: Check-Direct-Link Fired
 Time  2.466: Node-1-1-1-To-Node-1-1 Retrieved
 Time  2.466: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-1-1 - ADVANCING 
 Time  2.516: Direct-Link Fired
 Time  2.516: Arrived Selected
 ARRIVED AT TARGET Node-1-1 - CHECKING ANY SUBGOALS REMAINING 
 Time  2.566: Arrived Fired
 Time  2.566: Goal2403691 Retrieved
 Time  2.566: Returning Selected
 RETURNING TO GOAL Goal2403691 CURRENT LOCATION Node-1-1 TARGET LOCATION Node-1 
 Time  2.616: Returning Fired
 Time  2.616: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1 AND Node-1 
 Time  2.666: Check-Direct-Link Fired
 Time  2.666: Node-1-1-To-Node-1 Retrieved
 Time  2.666: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-1-1 AND Node-1 - ADVANCING 
 Time  2.716: Direct-Link Fired
 Time  2.716: Arrived Selected
 ARRIVED AT TARGET Node-1 - CHECKING ANY SUBGOALS REMAINING 
 Time  2.766: Arrived Fired
 Time  2.766: Goal2403690 Retrieved
 Time  2.766: Returning Selected
 RETURNING TO GOAL Goal2403690 CURRENT LOCATION Node-1 TARGET LOCATION Node-1-2-2 
 Time  2.816: Returning Fired
 Time  2.816: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node-1-2-2 
 Time  2.866: Check-Direct-Link Fired
 Time  3.866: Failure Retrieved
 Time  3.866: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node-1-2-2 - SUBGOALING 
 Time  3.916: No-Direct-Link Fired
 Time  3.916: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Node-1 AND Node-1-2-2 
 Time  3.966: Find-Intermediate-Location Fired
 Time  3.976: Node-1-2 Retrieved
 Time  3.976: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Node-1-2 
 Time  4.026: Subgoal-Intermdiate-Location Fired
 Time  4.026: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node-1-2 
 Time  4.076: Check-Direct-Link Fired
 Time  4.076: Node-1-To-Node-1-2 Retrieved
 Time  4.076: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node-1-2 - ADVANCING 
 Time  4.126: Direct-Link Fired
 Time  4.126: Arrived Selected
 ARRIVED AT TARGET Node-1-2 - CHECKING ANY SUBGOALS REMAINING 
 Time  4.176: Arrived Fired
 Time  4.176: Goal2403690 Retrieved
 Time  4.176: Returning Selected
 RETURNING TO GOAL Goal2403690 CURRENT LOCATION Node-1-2 TARGET LOCATION Node-1-2-2 
 Time  4.226: Returning Fired
 Time  4.226: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-2 AND Node-1-2-2 
 Time  4.276: Check-Direct-Link Fired
 Time  4.276: Node-1-2-To-Node-1-2-2 Retrieved
 Time  4.276: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-1-2 AND Node-1-2-2 - ADVANCING 
 Time  4.326: Direct-Link Fired
 Time  4.326: Arrived Selected
 ARRIVED AT TARGET Node-1-2-2 - CHECKING ANY SUBGOALS REMAINING 
 Time  4.376: Arrived Fired
 Time  5.376: Failure Retrieved
 Time  5.376: Completed Selected
 NO MORE SUBGOALS - PLANNING COMPLETED 
 Time  5.426: Completed Fired
(NODE-1-1-1 NODE-1-1 NODE-1 NODE-1-2 NODE-1-2-2)

CL-USER 456 > (plan 'node-1-1-1 'node-2-2-2)
 Time  5.426: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-2-2-2 
 Time  5.476: Check-Direct-Link Fired
 Time  6.476: Failure Retrieved
 Time  6.476: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-2-2-2 - SUBGOALING 
 Time  6.526: No-Direct-Link Fired
 Time  6.526: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Node-1-1-1 AND Node-2-2-2 
 Time  6.576: Find-Intermediate-Location Fired
 Time  6.586: Node-2-2 Retrieved
 Time  6.586: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Node-2-2 
 Time  6.636: Subgoal-Intermdiate-Location Fired
 Time  6.636: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-2-2 
 Time  6.686: Check-Direct-Link Fired
 Time  7.686: Failure Retrieved
 Time  7.686: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-2-2 - SUBGOALING 
 Time  7.736: No-Direct-Link Fired
 Time  7.736: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Node-1-1-1 AND Node-2-2 
 Time  7.786: Find-Intermediate-Location Fired
 Time  7.796: Node-1-1 Retrieved
 Time  7.796: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Node-1-1 
 Time  7.846: Subgoal-Intermdiate-Location Fired
 Time  7.846: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-1-1 
 Time  7.896: Check-Direct-Link Fired
 Time  7.896: Node-1-1-1-To-Node-1-1 Retrieved
 Time  7.896: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-1-1-1 AND Node-1-1 - ADVANCING 
 Time  7.946: Direct-Link Fired
 Time  7.946: Arrived Selected
 ARRIVED AT TARGET Node-1-1 - CHECKING ANY SUBGOALS REMAINING 
 Time  7.996: Arrived Fired
 Time  7.996: Goal2403695 Retrieved
 Time  7.996: Returning Selected
 RETURNING TO GOAL Goal2403695 CURRENT LOCATION Node-1-1 TARGET LOCATION Node-2-2 
 Merging chunk Goal2403696 into chunk Goal2403692
 Time  8.046: Returning Fired
 Time  8.046: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1 AND Node-2-2 
 Time  8.096: Check-Direct-Link Fired
 Time  9.096: Failure Retrieved
 Time  9.096: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Node-1-1 AND Node-2-2 - SUBGOALING 
 Time  9.146: No-Direct-Link Fired
 Time  9.146: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Node-1-1 AND Node-2-2 
 Time  9.196: Find-Intermediate-Location Fired
 Time  9.208: Node-1 Retrieved
 Time  9.208: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Node-1 
 Time  9.258: Subgoal-Intermdiate-Location Fired
 Time  9.258: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1-1 AND Node-1 
 Time  9.308: Check-Direct-Link Fired
 Time  9.308: Node-1-1-To-Node-1 Retrieved
 Time  9.308: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-1-1 AND Node-1 - ADVANCING 
 Time  9.358: Direct-Link Fired
 Time  9.358: Arrived Selected
 ARRIVED AT TARGET Node-1 - CHECKING ANY SUBGOALS REMAINING 
 Time  9.408: Arrived Fired
 Time  9.408: Goal2403695 Retrieved
 Time  9.408: Returning Selected
 RETURNING TO GOAL Goal2403695 CURRENT LOCATION Node-1 TARGET LOCATION Node-2-2 
 Merging chunk Goal2403697 into chunk Goal2403691
 Time  9.458: Returning Fired
 Time  9.458: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node-2-2 
 Time  9.508: Check-Direct-Link Fired
 Time 10.508: Failure Retrieved
 Time 10.508: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node-2-2 - SUBGOALING 
 Time 10.558: No-Direct-Link Fired
 Time 10.558: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Node-1 AND Node-2-2 
 Time 10.608: Find-Intermediate-Location Fired
 Time 10.619: Node-2 Retrieved
 Time 10.619: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Node-2 
 Time 10.669: Subgoal-Intermdiate-Location Fired
 Time 10.669: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node-2 
 Time 10.719: Check-Direct-Link Fired
 Time 11.719: Failure Retrieved
 Time 11.719: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node-2 - SUBGOALING 
 Time 11.769: No-Direct-Link Fired
 Time 11.769: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Node-1 AND Node-2 
 Time 11.819: Find-Intermediate-Location Fired
 Time 11.833: Node Retrieved
 Time 11.833: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Node 
 Time 11.883: Subgoal-Intermdiate-Location Fired
 Time 11.883: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node 
 Time 11.933: Check-Direct-Link Fired
 Time 11.933: Node-1-To-Node Retrieved
 Time 11.933: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-1 AND Node - ADVANCING 
 Time 11.983: Direct-Link Fired
 Time 11.983: Arrived Selected
 ARRIVED AT TARGET Node - CHECKING ANY SUBGOALS REMAINING 
 Time 12.033: Arrived Fired
 Time 12.033: Goal2403698 Retrieved
 Time 12.033: Returning Selected
 RETURNING TO GOAL Goal2403698 CURRENT LOCATION Node TARGET LOCATION Node-2 
 Time 12.083: Returning Fired
 Time 12.083: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node AND Node-2 
 Time 12.133: Check-Direct-Link Fired
 Time 12.133: Node-To-Node-2 Retrieved
 Time 12.133: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node AND Node-2 - ADVANCING 
 Time 12.183: Direct-Link Fired
 Time 12.183: Arrived Selected
 ARRIVED AT TARGET Node-2 - CHECKING ANY SUBGOALS REMAINING 
 Time 12.233: Arrived Fired
 Time 12.233: Goal2403695 Retrieved
 Time 12.233: Returning Selected
 RETURNING TO GOAL Goal2403695 CURRENT LOCATION Node-2 TARGET LOCATION Node-2-2 
 Time 12.283: Returning Fired
 Time 12.283: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-2 AND Node-2-2 
 Time 12.333: Check-Direct-Link Fired
 Time 12.333: Node-2-To-Node-2-2 Retrieved
 Time 12.333: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-2 AND Node-2-2 - ADVANCING 
 Time 12.383: Direct-Link Fired
 Time 12.383: Arrived Selected
 ARRIVED AT TARGET Node-2-2 - CHECKING ANY SUBGOALS REMAINING 
 Time 12.433: Arrived Fired
 Time 12.433: Goal2403694 Retrieved
 Time 12.433: Returning Selected
 RETURNING TO GOAL Goal2403694 CURRENT LOCATION Node-2-2 TARGET LOCATION Node-2-2-2 
 Time 12.483: Returning Fired
 Time 12.483: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Node-2-2 AND Node-2-2-2 
 Time 12.533: Check-Direct-Link Fired
 Time 12.533: Node-2-2-To-Node-2-2-2 Retrieved
 Time 12.533: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Node-2-2 AND Node-2-2-2 - ADVANCING 
 Time 12.583: Direct-Link Fired
 Time 12.583: Arrived Selected
 ARRIVED AT TARGET Node-2-2-2 - CHECKING ANY SUBGOALS REMAINING 
 Time 12.633: Arrived Fired
 Time 13.633: Failure Retrieved
 Time 13.633: Completed Selected
 NO MORE SUBGOALS - PLANNING COMPLETED 
 Time 13.683: Completed Fired
(NODE-1-1-1 NODE-1-1 NODE-1 NODE NODE-2 NODE-2-2 NODE-2-2-2)
CL-USER 508 > (reset-model-parameters (make-grid 5 5))
25

CL-USER 509 > (plan 'grid-1-1 'grid-4-5)
 Time  0.000: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-1-1 AND Grid-4-5 
 Time  0.050: Check-Direct-Link Fired
 Time  1.050: Failure Retrieved
 Time  1.050: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Grid-1-1 AND Grid-4-5 - SUBGOALING 
 Time  1.100: No-Direct-Link Fired
 Time  1.100: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Grid-1-1 AND Grid-4-5 
 Time  1.150: Find-Intermediate-Location Fired
 Time  1.171: Grid-1-2 Retrieved
 Time  1.171: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Grid-1-2 
 Time  1.221: Subgoal-Intermdiate-Location Fired
 Time  1.221: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-1-1 AND Grid-1-2 
 Time  1.271: Check-Direct-Link Fired
 Time  1.271: Grid-1-1-To-Grid-1-2 Retrieved
 Time  1.271: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Grid-1-1 AND Grid-1-2 - ADVANCING 
 Time  1.321: Direct-Link Fired
 Time  1.321: Arrived Selected
 ARRIVED AT TARGET Grid-1-2 - CHECKING ANY SUBGOALS REMAINING 
 Time  1.371: Arrived Fired
 Time  1.371: Goal2406098 Retrieved
 Time  1.371: Returning Selected
 RETURNING TO GOAL Goal2406098 CURRENT LOCATION Grid-1-2 TARGET LOCATION Grid-4-5 
 Time  1.421: Returning Fired
 Time  1.421: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-1-2 AND Grid-4-5 
 Time  1.471: Check-Direct-Link Fired
 Time  2.471: Failure Retrieved
 Time  2.471: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Grid-1-2 AND Grid-4-5 - SUBGOALING 
 Time  2.521: No-Direct-Link Fired
 Time  2.521: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Grid-1-2 AND Grid-4-5 
 Time  2.571: Find-Intermediate-Location Fired
 Time  2.594: Grid-2-2 Retrieved
 Time  2.594: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Grid-2-2 
 Time  2.644: Subgoal-Intermdiate-Location Fired
 Time  2.644: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-1-2 AND Grid-2-2 
 Time  2.694: Check-Direct-Link Fired
 Time  2.694: Grid-1-2-To-Grid-2-2 Retrieved
 Time  2.694: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Grid-1-2 AND Grid-2-2 - ADVANCING 
 Time  2.744: Direct-Link Fired
 Time  2.744: Arrived Selected
 ARRIVED AT TARGET Grid-2-2 - CHECKING ANY SUBGOALS REMAINING 
 Time  2.794: Arrived Fired
 Time  2.794: Goal2406098 Retrieved
 Time  2.794: Returning Selected
 RETURNING TO GOAL Goal2406098 CURRENT LOCATION Grid-2-2 TARGET LOCATION Grid-4-5 
 Time  2.844: Returning Fired
 Time  2.844: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-2-2 AND Grid-4-5 
 Time  2.894: Check-Direct-Link Fired
 Time  3.894: Failure Retrieved
 Time  3.894: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Grid-2-2 AND Grid-4-5 - SUBGOALING 
 Time  3.944: No-Direct-Link Fired
 Time  3.944: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Grid-2-2 AND Grid-4-5 
 Time  3.994: Find-Intermediate-Location Fired
 Time  4.017: Grid-4-4 Retrieved
 Time  4.017: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Grid-4-4 
 Time  4.067: Subgoal-Intermdiate-Location Fired
 Time  4.067: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-2-2 AND Grid-4-4 
 Time  4.117: Check-Direct-Link Fired
 Time  5.117: Failure Retrieved
 Time  5.117: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Grid-2-2 AND Grid-4-4 - SUBGOALING 
 Time  5.167: No-Direct-Link Fired
 Time  5.167: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Grid-2-2 AND Grid-4-4 
 Time  5.217: Find-Intermediate-Location Fired
 Time  5.252: Grid-2-3 Retrieved
 Time  5.252: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Grid-2-3 
 Time  5.302: Subgoal-Intermdiate-Location Fired
 Time  5.302: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-2-2 AND Grid-2-3 
 Time  5.352: Check-Direct-Link Fired
 Time  5.352: Grid-2-2-To-Grid-2-3 Retrieved
 Time  5.352: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Grid-2-2 AND Grid-2-3 - ADVANCING 
 Time  5.402: Direct-Link Fired
 Time  5.402: Arrived Selected
 ARRIVED AT TARGET Grid-2-3 - CHECKING ANY SUBGOALS REMAINING 
 Time  5.452: Arrived Fired
 Time  5.452: Goal2406101 Retrieved
 Time  5.452: Returning Selected
 RETURNING TO GOAL Goal2406101 CURRENT LOCATION Grid-2-3 TARGET LOCATION Grid-4-4 
 Time  5.502: Returning Fired
 Time  5.502: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-2-3 AND Grid-4-4 
 Time  5.552: Check-Direct-Link Fired
 Time  6.552: Failure Retrieved
 Time  6.552: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Grid-2-3 AND Grid-4-4 - SUBGOALING 
 Time  6.602: No-Direct-Link Fired
 Time  6.602: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Grid-2-3 AND Grid-4-4 
 Time  6.652: Find-Intermediate-Location Fired
 Time  6.673: Grid-3-3 Retrieved
 Time  6.673: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Grid-3-3 
 Time  6.723: Subgoal-Intermdiate-Location Fired
 Time  6.723: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-2-3 AND Grid-3-3 
 Time  6.773: Check-Direct-Link Fired
 Time  6.773: Grid-2-3-To-Grid-3-3 Retrieved
 Time  6.773: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Grid-2-3 AND Grid-3-3 - ADVANCING 
 Time  6.823: Direct-Link Fired
 Time  6.823: Arrived Selected
 ARRIVED AT TARGET Grid-3-3 - CHECKING ANY SUBGOALS REMAINING 
 Time  6.873: Arrived Fired
 Time  6.873: Goal2406101 Retrieved
 Time  6.873: Returning Selected
 RETURNING TO GOAL Goal2406101 CURRENT LOCATION Grid-3-3 TARGET LOCATION Grid-4-4 
 Time  6.923: Returning Fired
 Time  6.923: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-3-3 AND Grid-4-4 
 Time  6.973: Check-Direct-Link Fired
 Time  7.973: Failure Retrieved
 Time  7.973: No-Direct-Link Selected
 NO DIRECT LINK BETWEEN LOCATIONS Grid-3-3 AND Grid-4-4 - SUBGOALING 
 Time  8.023: No-Direct-Link Fired
 Time  8.023: Find-Intermediate-Location Selected
 RETRIEVING INTERMEDIATE LOCATION BETWEEN Grid-3-3 AND Grid-4-4 
 Time  8.073: Find-Intermediate-Location Fired
 Time  8.088: Grid-3-4 Retrieved
 Time  8.088: Subgoal-Intermdiate-Location Selected
 SUBGOALING BETTING TO INTERMEDIATE LOCATION Grid-3-4 
 Time  8.138: Subgoal-Intermdiate-Location Fired
 Time  8.138: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-3-3 AND Grid-3-4 
 Time  8.188: Check-Direct-Link Fired
 Time  8.188: Grid-3-3-To-Grid-3-4 Retrieved
 Time  8.188: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Grid-3-3 AND Grid-3-4 - ADVANCING 
 Time  8.238: Direct-Link Fired
 Time  8.238: Arrived Selected
 ARRIVED AT TARGET Grid-3-4 - CHECKING ANY SUBGOALS REMAINING 
 Time  8.288: Arrived Fired
 Time  8.288: Goal2406101 Retrieved
 Time  8.288: Returning Selected
 RETURNING TO GOAL Goal2406101 CURRENT LOCATION Grid-3-4 TARGET LOCATION Grid-4-4 
 Time  8.338: Returning Fired
 Time  8.338: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-3-4 AND Grid-4-4 
 Time  8.388: Check-Direct-Link Fired
 Time  8.388: Grid-3-4-To-Grid-4-4 Retrieved
 Time  8.388: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Grid-3-4 AND Grid-4-4 - ADVANCING 
 Time  8.438: Direct-Link Fired
 Time  8.438: Arrived Selected
 ARRIVED AT TARGET Grid-4-4 - CHECKING ANY SUBGOALS REMAINING 
 Time  8.488: Arrived Fired
 Time  8.488: Goal2406098 Retrieved
 Time  8.488: Returning Selected
 RETURNING TO GOAL Goal2406098 CURRENT LOCATION Grid-4-4 TARGET LOCATION Grid-4-5 
 Time  8.538: Returning Fired
 Time  8.538: Check-Direct-Link Selected
 CHECKING FOR DIRECT LINK BETWEEN LOCATIONS Grid-4-4 AND Grid-4-5 
 Time  8.588: Check-Direct-Link Fired
 Time  8.588: Grid-4-4-To-Grid-4-5 Retrieved
 Time  8.588: Direct-Link Selected
 DIRECT LINK BETWEEN LOCATIONS Grid-4-4 AND Grid-4-5 - ADVANCING 
 Time  8.638: Direct-Link Fired
 Time  8.638: Arrived Selected
 ARRIVED AT TARGET Grid-4-5 - CHECKING ANY SUBGOALS REMAINING 
 Time  8.688: Arrived Fired
 Time  9.688: Failure Retrieved
 Time  9.688: Completed Selected
 NO MORE SUBGOALS - PLANNING COMPLETED 
 Time  9.738: Completed Fired
(GRID-1-1 GRID-1-2 GRID-2-2 GRID-2-3 GRID-3-3 GRID-3-4 GRID-4-4 GRID-4-5)

CL-USER 510 >  
|#