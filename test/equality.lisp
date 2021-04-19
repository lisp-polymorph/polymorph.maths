;;; Equality comparison unit tests

(defpackage #:polymorph.maths/test.equality
  (:use #:cl #:alexandria #:fiveam #:polymorph.maths/test)
  (:shadowing-import-from
   #:polymorph.maths
    #:= #:/=
    #:< #:<= #:> #:>=
    #:+ #:- #:* #:/))

(in-package #:polymorph.maths/test.equality)

;;; Test suite definition

(def-suite equality
    :description "Equality comparison tests"
    :in polymorph.maths)

(in-suite equality)


;;; Tests

;;;; Numbers

(test number-=
  :description "Test `=` on numbers"

  (is (= 1 1))
  (is (= 2.0 2))
  (is (= 6/3 2))

  (is (not (= 1 0)))
  (is (not (= 1 'x)))
  (is (not (= 1 #\1)))

  (is (= 1))
  (is (= 1 1.0 2/2))
  (is (not (= 1 "1" #\1))))

(test number-/=
  :description "Test `/=` on numbers"

  (is (/= 1 0))
  (is (/= 2 3))
  (is (/= 1))
  (is (/= 1 "1" #\1))
  (is (not (/= 1 1.0 2/2))))


;;;; Characters

(test character-=
  :description "Test `=` on characters"

  (is (= #\a #\a))
  (is (= #\0 #\0))

  (is (not (= #\a #\A)))
  (is (not (= #\a 'a)))
  (is (not (= #\a "a")))

  (is (= #\a #\a #\a))
  (is (not (= #\a #\A 'a))))

(test character-/=
  :description "Test `/=` on characters"

  (is (/= #\a #\A))
  (is (/= #\x #\y))
 (is (/= #\a 'a))
 (is (/= #\a "a"))

  (is (/= #\a 'a "a"))
  (is (not (/= #\a #\a #\a))))


;;;; Conses/Lists

(test cons-=
  :description "Test `=` on lists and cons"

  (is (= '(1 2 3) (list 1.0 2 3.0)))
  (is (= '(1 a #\x) (list 2/2 'a #\x)))
  (is (= '(((1 2) x y) #\z) (list (list (list 1 2) 'x 'y) #\z)))

  (is (not (= '(1 2 3) '(1 2 1))))
  (is (not (= '(1 2 3) '(1 2))))
  (is (not (= '(1 2 3) '(1 2 . 3)))))


;;; Single-dimensional Arrays (Vector)

(test vector-=
  :description "Test `=` on single dimensional arrays (vectors)"

  (is (= #(1 2 3) (vector 1 2 3)))
  (is (= #(1 2 3) (make-array 3
                              :element-type 'number
                              :adjustable t
                              :fill-pointer t
                              :initial-contents '(1 2 3))))

  (is (= #(1 2 x) (vector 1.0 2 'x)))
  (is (= #(#(1 2) 3) (vector (vector 1.0 2.0) 3)))
  (is (= #((1 2) 3) (vector '(1.0 2.0) 3)))

  (is (not (= #(1 2 3) #(1 1 1))))
  (is (not (= #(1 2 3) #(1 2 3 4))))
  (is (not (= #(1 2 3) (make-array 0))))
  (is (not (= #(1 2 3) (make-array '(2 2) :initial-contents '((1 2) (3 4))))))
  (is (not (= #(#(1 2)) #(#(2 1))))))


;;; Multi-dimensional Arrays

(test nd-array-=
  :description "Test `=` on multi-dimensional arrays"

  (is (= #2A((1 2 3) (4 5 6)) (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
  (is (= #2A((1 (3 4)) (5 #\c)) (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c)))))

  (is (not (= #2A((1 2) (3 4)) #2A((1 1) (3 4)))))
  (is (not (= #2A((1 2) (3 4)) #(1 2 3 4)))))


;;; Strings

(test string-=
  :description "Test `=` on strings"

  (is (= "Hello" "Hello"))
  (is (= "World" (string '|World|)))
  (is (= "AAA" (make-string 3 :initial-element #\A)))
  (is (= "hello" (vector #\h #\e #\l #\l #\o)))

  (is (not (= "hello" "Hello")))
  (is (not (= "hello" '|hello|)))
  (is (not (= "world" "worlds"))))


;;; Hash-Tables

(test hash-table-=
  :description "Test `=` on hash-tables"

  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash 'x table) 1)
    (setf (gethash 'y table) 'z)
    (setf (gethash "hello" table) "world")
    (setf (gethash '(1 2 3) table) #\z)

    (is (= table
           (alist-hash-table
            '((x . 1) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is (= table
           (alist-hash-table
            '((x . 1) (y . z) ("HELLO" . "world") ((1 2 3) . #\z)) :test #'equalp)))

    (is (not
         (= table
            (alist-hash-table
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z))))))

    (is (not
         (= table
            (alist-hash-table
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z))))))

    (is (not
         (= table
            (alist-hash-table
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z) ("x" . "z"))))))))


;;; Symbols

(test symbol-=
  :description "Test `=` on symbols"

  (is (= 'a 'a))
  (is (= 'sym 'sym))
  (is (= '|a symbol| '|a symbol|))
  (is (= :key :key))

  (is (not (= 'a 'b)))
  (is (not (= 'sym 'syms)))
  (is (not (= :key1 :key2)))
  (is (not (= 'a :a)))
  (is (not (= 'a '#:a))))
