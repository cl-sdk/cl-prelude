(asdf:defsystem #:cl-prelude.test
  :author "Bruno Dias"
  :version "0.0.1"
  :license "Unlicense"
  :serial t
  :depends-on (#:fiveam
	       #:cl-prelude)
  :components ((:file "test")))
