(asdf:defsystem :ball-z-2d
  :description "Ball Z: Second Dimension"
  :license "AGPLv3"
  :version "1.0.4"
  :author "Pavel 'Borodust' Korolev"
  :mailto "dev@borodust.org"
  :depends-on (alexandria split-sequence parse-number cl-svg-polygon
                          trivial-gamekit cl-bodge/physics/2d cl-bodge/physics)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "camera")
               (:file "ball")
               (:file "level")
               (:file "state")
               (:file "ball-z")
               (:module "game"
                :serial t
                :components ((:file "start")
                             (:file "play")
                             (:file "fail")
                             (:file "victory")))))
