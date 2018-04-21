(asdf:defsystem :ball-z-2d
  :description "Ball Z: Second Dimension"
  :license "AGPLv3"
  :version "0.0.1"
  :author "Pavel 'Borodust' Korolev"
  :mailto "dev@borodust.org"
  :depends-on (alexandria parse-number cl-svg-polygon
                          trivial-gamekit cl-bodge/physics/2d cl-bodge/physics)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "level")
               (:file "ball-z")))
