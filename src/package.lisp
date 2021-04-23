;;;; package.lisp

(defpackage #:polymorph.maths
  (:use #:cl #:adhoc-polymorphic-functions #:alexandria)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))
  (:shadow #:= #:/=
           #:< #:<= #:> #:>=
           #:max #:min
           #:+ #:- #:* #:/)
  (:export #:= #:/=
           #:< #:<= #:> #:>=
           #:+ #:- #:* #:/))
