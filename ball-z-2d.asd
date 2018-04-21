(asdf:defsystem :ball-z-2d
  :description "Ball Z: Second Dimension"
  :license "AGPLv3"
  :version "0.0.1"
  :author "Pavel 'Borodust' Korolev"
  :mailto "dev@borodust.org"
  :depends-on (trivial-gamekit)
  :serial t
  :components ((:file "packages")
               (:file "ball-z")))
