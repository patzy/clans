;;;; clans.asd

(asdf:defsystem #:clans
  :serial t
  :depends-on (#:glaw
               #:glop
               #:glaw-imago)
  :components ((:file "package")
               (:file "constants")
               (:file "screens")
               (:file "loading")
               (:file "game")
               (:file "title")
               (:file "clans")))

