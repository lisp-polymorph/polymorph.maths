;;;; package.lisp

(defpackage #:polymorph.maths
  (:use #:cl #:adhoc-polymorphic-functions #:alexandria #:polymorph.utility)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))
  (:shadow #:= #:/=
           #:< #:<= #:> #:>=
           #:max #:min
           #:+ #:- #:* #:/)
  (:export #:= #:/=
           #:< #:<= #:> #:>=
           #:max #:min
           #:+ #:- #:* #:/))
