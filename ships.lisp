(in-package #:hati-server)

(defclass ship ()
	((id
	  :initarg :id)
	 (chunk-x
	  :initarg :chunk-x)
	 (chunk-y
	  :initarg :chunk-y)
	 (internal-x
	  :initarg :internal-x)
	 (internal-y
	  :initarg :internal-y)
	 (velocity-x
	  :initarg :velocity-x
	  :initform 0)
	 (velocity-y
	  :initarg :velocity-y
	  :initform 0)
	 (acceleration-x
	  :initarg :acceleration-x
	  :initform 0)
	 (acceleration-y
	  :initarg :acceleration-y
	  :initform 0)))

(defvar *ships* ())
(defvar *ship-id* 0)

(defun make-ship (chunk-x chunk-y internal-x internal-y)
	(let ((ship
		(make-instance 'ship
			       :id *ship-id*
			       :chunk-x chunk-x
			       :chunk-y chunk-y
			       :internal-x internal-x
			       :internal-y internal-y)))
		;; Ensure uniqueness of IDs
		(incf *ship-id*)
		;; Add ship to the list of ships
		(setf *ships* (cons ship *ships*))
		;; Place the ship in the appropriate chunk
		(enter-chunk ship chunk-x chunk-y)
		ship))
