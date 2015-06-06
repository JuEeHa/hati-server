;;;; hati-server.asd

(asdf:defsystem #:hati-server
  :description "Server software for the Hati spaceflight / resource management game"
  :author "Juhani Haverinen <juhani.haverinen@gmail.com>"
  :license "Unlicense"
  :depends-on (#:usocket)
  :serial t
  :components ((:file "package")
               (:file "hati-server")
	       (:file "chunks")
	       (:file "ships")))

