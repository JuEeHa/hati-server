;;;; Functions for operating on chunks.
;;;; The space is composed of several chunks, 256 x 256 in size, generated procedurally.
;;;; The chunks are loaded and unloaded as ships enter of leave them.

;;;; TODO: add the concept of gravity area and make chunks included in that count as referenced

(in-package #:hati-server)

(defclass chunk ()
	((ships
	  :initform ())
	 (references
	  :initform 0)))

(defvar *space* (make-hash-table :test 'equal))
(defparameter *chunk-width* 256)
(defparameter *chunk-height* 256)

(defun generate-chunk (chunk-x chunk-y)
	;; TODO: actually generate something
	(setf (gethash (cons chunk-x chunk-y) *space*) (make-instance 'chunk)))

(defun unload-chunk (chunk-x chunk-y)
	(remhash (cons chunk-x chunk-y) *space*))

(defun chunk-loaded-p (chunk-x chunk-y)
	(multiple-value-bind (value present) (gethash (cons chunk-x chunk-y) *space*)
		present))

(defun get-chunk (chunk-x chunk-y)
	(gethash (cons chunk-x chunk-y) *space*))

(defun require-chunk (chunk-x chunk-y)
	;; Create the chunk if it doesn't exist yet
	(unless (chunk-loaded-p chunk-x chunk-y)
		(generate-chunk chunk-x chunk-y))
	(let ((chunk (get-chunk chunk-x chunk-y)))
		;; Increase reference count
		(incf (slot-value chunk 'references))))

(defun unrequire-chunk (chunk-x chunk-y)
	(let ((chunk (get-chunk chunk-x chunk-y)))
		;; Decrease reference count
		(decf (slot-value chunk 'references))
		;; When chunk's reference count drops to 0, remove it
		(when (= (slot-value chunk 'references) 0)
			(unload-chunk chunk-x chunk-y))))

(defun enter-chunk (ship chunk-x chunk-y)
	;; Mark that we use this chunk
	(require-chunk chunk-x chunk-y)
	(let ((chunk (get-chunk chunk-x chunk-y)))
		;; Add ship to the list of ships
		(setf (slot-value chunk 'ships) (cons ship (slot-value chunk 'ships)))))

(defun leave-chunk (ship chunk-x chunk-y)
	(let ((chunk (get-chunk chunk-x chunk-y)))
		;; Remove ship from the list of ships
		(setf (slot-value chunk 'ships) (remove ship (slot-value chunk 'ships))))
	;; Mark that we no longer use this chunk
	(unrequire-chunk chunk-x chunk-y))

(defun ships-in-chunk (chunk-x chunk-y)
	(let ((chunk (get-chunk chunk-x chunk-y)))
		(slot-value chunk 'ships)))

(defun objects-in-chunk (chunk-x chunk-y)
	(ships-in-chunk chunk-x chunk-y))
