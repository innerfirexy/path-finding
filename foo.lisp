;; demo code for cl-json

(load "~/quicklisp/setup.lisp")
(ql:quickload 'cl-json)

;; (setq x (json:encode-json '#( ((foo . (1 2 3)) (bar . t) (baz . #\!)) "quux" 4/17 4.25)))
;; (with-input-from-string (s "{\"foo\": [1, 2, 3], \"bar\": true, \"baz\": \"!\"}") (json:decode-json s))

;; graphs
(ql:quickload 'vecto)
(ql:quickload 'cl-vectors)
(ql:quickload 'skippy)


(defun convert-to-gif (filename &optional force)
  (if (equal (subseq filename (- (length filename) 4)) ".gif")
      filename ;; no-op if it's already a GIF
      (let ((target (concatenate 'string (subseq filename 0 (- (length filename) 4)) ".gif")))
	(or (and (not force) (if (probe-file target) target))
	    (progn (format t "Converting ~a to GIF file." filename)
		   (ccl::run-program "convert" (list filename target) :output t )
		   target)))))

(defun compose-images (background foreground target)
	(ccl::run-program "composite" (list foreground background target) :output t ))

(defun foo-draw (input-file)
	(let* ((mid-file (concatenate 'string (subseq input-file 0 (search ".png" input-file)) "-foo-mid.png"))
		(output-file (concatenate 'string (subseq input-file 0 (search ".png" input-file)) "-foo-output.png"))
		(stream (skippy::load-data-stream (convert-to-gif input-file)))
		(image (aref (skippy::images stream) 0))
		(canvas-width (skippy::width image))
		(canvas-height (skippy::height image)))
		(vecto::with-canvas (:width canvas-width :height canvas-height)
			(vecto::set-rgb-stroke 0 1 0)
			(vecto::with-graphics-state
				(vecto::move-to 30 60)
				(vecto::line-to 210 100)
				(vecto::stroke))
			(vecto::save-png mid-file))
		(compose-images input-file mid-file output-file)))

(defun foo ()
	(let* ((stream (skippy::load-data-stream "p2s2.gif"))
		(image (aref (skippy::images stream) 0))
		(*canvas-width* (skippy::width image))
		(*canvas-height* (skippy::height image)))
		(vecto::with-canvas (:width *canvas-width* :height *canvas-height*)
			(vecto::set-rgb-stroke 0 1 0)
			(vecto::with-graphics-state
				(vecto::move-to 30 60)
				(vecto::line-to 210 100)
				(vecto::stroke))
			(vecto::save-png "foo.png"))
		(ccl::run-program "composite" (list "foo.png" "p2s2.gif" "foo-output.png") :output t)))