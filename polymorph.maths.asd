;;;; polymorph.maths.asd

(asdf:defsystem #:polymorph.maths
    :description "Maths for polymorph.stl"
    :author "Commander Thrashdin"
    :license  "MIT"
    :version "0.5"
    :serial t
    :depends-on (#:adhoc-polymorphic-functions #:compiler-macro)
    :components ((:module
		  "src"
		  :serial t
		  :components
		  ((:file "package")
                   (:file "polymorph.maths")))))
