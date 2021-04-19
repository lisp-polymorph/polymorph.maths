;;; Unit tests for polymorph.maths

(defpackage #:polymorph.maths/test
  (:use #:cl #:alexandria #:fiveam)
  (:export #:polymorph.maths
           #:test-polymorph.maths)

  (:shadowing-import-from
   #:polymorph.maths
    #:= #:/=
    #:< #:<= #:> #:>=
    #:+ #:- #:* #:/))

(in-package #:polymorph.maths/test)

;;; Test suite definition

(def-suite polymorph.maths
    :description "Master test suite for polymorph.maths")

(in-suite polymorph.maths)

(defun test-polymorph.maths ()
  (run! 'polymorph.maths))
