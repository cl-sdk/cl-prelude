(asdf:defsystem #:cl-prelude
  :author "Bruno Dias"
  :version "0.0.1"
  :license "Unlicense"
  :serial t
  :depends-on (#:cl-algebraic-data-type)
  :components ((:file "package")
	       (:file "ordering")
	       (:file "maybe")
	       (:file "either")
	       (:file "result")
	       (:file "validation")))
