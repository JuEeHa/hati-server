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

(defun move-ship (ship new-chunk-x new-chunk-y new-internal-x new-internal-y)
	(with-slots (chunk-x chunk-y internal-x internal-y) ship
		(enter-chunk ship new-chunk-x new-chunk-y)
		(leave-chunk ship chunk-x chunk-y)
		;; Upgrade position information
		(setf chunk-x new-chunk-x)
		(setf chunk-y new-chunk-y)
		(setf internal-x new-internal-x)
		(setf internal-y new-internal-y)))
