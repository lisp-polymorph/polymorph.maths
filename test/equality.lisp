;;; Equality comparison unit tests

(defpackage #:polymorph.maths/test.equality
  (:use #:cl #:alexandria #:fiveam #:polymorph.maths/test)
  (:shadowing-import-from
   #:polymorph.maths
    #:= #:/=
    #:< #:<= #:> #:>=
    #:+ #:- #:* #:/)

  (:import-from #:adhoc-polymorphic-functions
                #:no-applicable-polymorph))

(in-package #:polymorph.maths/test.equality)

;;; Test suite definition

(def-suite equality
    :description "Equality comparison tests"
    :in polymorph.maths)

(in-suite equality)


;;; Tests

;;;; Numbers

(test number-=
  "Test `=` on numbers"

  (is (= 1 1))
  (is (= 2.0 2))
  (is (= 6/3 2))

  (is (not (= 1 0)))
  (is (not (= 2.3 5.7)))

  (is (= 1))
  (is (= 1 1.0 2/2))
  (is (not (= 1 1 3))))

(test number-/=
  "Test `/=` on numbers"

  (is (/= 1 0))
  (is (/= 2 3))
  (is (/= 1))
  (is (/= 1 1 3))
  (is (not (/= 1 1.0 2/2))))

(test random-number-=
  "Test `=` on random numbers"

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:= a b)
            (= a b)))

    (is (eq (= b a) (= a b)))))

(test random-number-/=
  "Test `/=` on random numbers"

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:/= a b)
            (/= a b)))

    (is (eq (/= b a) (/= a b)))))


;;;; Characters

(test character-=
  "Test `=` on characters"

  (is (= #\a #\a))
  (is (= #\0 #\0))
  (is (= #\b (char (make-string 5 :initial-element #\b) 3)))
  (is (= #\y (aref (make-array 3 :element-type 'character :initial-contents '(#\x #\y #\z)) 1)))

  (is (not (= #\a #\A)))
  (is (not (= #\x (char (make-string 5 :initial-element #\b) 3))))
  (is (not (= #\z (aref (make-array 3 :element-type 'character :initial-contents '(#\x #\y #\z)) 1))))

  (is (= #\a #\a #\a))
  (is (not (= #\a #\A 'a))))

(test random-character-=
  "Test `=` on random characters"

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char= a b)
            (= a b)))

    (is (eq (= b a) (= a b)))))

(test character-/=
  "Test `/=` on characters"

  (is (/= #\a #\A))
  (is (/= #\x #\y))
  (is (/= #\x (char (make-string 5 :initial-element #\b) 3)))
  (is (/= #\z (aref (make-array 3 :element-type 'character :initial-contents '(#\x #\y #\z)) 1)))
  (is (/= #\d #\d #\e))

  (is (not (/= #\a #\a #\a)))
  (is (not (/= #\b (char (make-string 5 :initial-element #\b) 3))))
  (is (not (/= #\y (aref (make-array 3 :element-type 'character :initial-contents '(#\x #\y #\z)) 1)))))

(test random-character-/=
  "Test `/=` on random characters"

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char/= a b)
            (/= a b)))

    (is (eq (/= b a) (/= a b)))))


;;;; Conses/Lists

(test cons-=
  "Test `=` on lists and cons"

  (is (= '(1 2 3) (list 1.0 2 3.0)))
  (is (= '(a b c) (cons 'a (cons 'b (cons 'c nil)))))
  (is (= '(1 a #\x) (list 2/2 'a #\x)))
  (is (= '(((1 2) x y) #\z) (list (list (list 1 2) 'x 'y) #\z)))
  (is (= '("abc" "def") (list "abc" "def")))
  (is (= nil (cdr (list '1))))
  (is (= '(5 6 . 3) '(5.0 6 . 3.0)))
  (is (= (make-list 100) (make-list 100)))

  (is (not (= '(1 2 3) '(1 2 1))))
  (is (not (= '(1 2 3) '(1 2)))))

(test cons-/=
  "Test `/=` on lists and cons"

  (is (not (/= '(1 2 3) (list 1.0 2 3.0))))
  (is (not (/= '(a b c) (cons 'a (cons 'b (cons 'c nil))))))
  (is (not (/= '(1 a #\x) (list 2/2 'a #\x))))
  (is (not (/= '(((1 2) x y) #\z) (list (list (list 1 2) 'x 'y) #\z))))
  (is (not (/= '("abc" "def") (list "abc" "def"))))
  (is (not (/= nil (cdr (list '1)))))
  (is (not (/= '(5 6 . 3) '(5.0 6 . 3.0))))

  (is (/= '(1 2 3) '(1 2 1)))
  (is (/= '(1 2 3) '(1 2))))


;;; Single-dimensional Arrays (Vector)

(test vector-=
  "Test `=` on single dimensional arrays (vectors)"

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

(test vector-/=
  "Test `/=` on single dimensional arrays (vectors)"

  (is (not (/= #(1 2 3) (vector 1 2 3))))
  (is (not (/= #(1 2 3) (make-array 3
                                :element-type 'number
                                :adjustable t
                                :fill-pointer t
                                :initial-contents '(1 2 3)))))

  (is (not (/= #(1 2 x) (vector 1.0 2 'x))))
  (is (not (/= #(#(1 2) 3) (vector (vector 1.0 2.0) 3))))
  (is (not (/= #((1 2) 3) (vector '(1.0 2.0) 3))))

  (is (/= #(1 2 3) #(1 1 1)))
  (is (/= #(1 2 3) #(1 2 3 4)))
  (is (/= #(1 2 3) (make-array 0)))
  (is (/= #(1 2 3) (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
  (is (/= #(#(1 2)) #(#(2 1)))))


;;; Multi-dimensional Arrays

(test nd-array-=
  "Test `=` on multi-dimensional arrays"

  (is (= #2A((1 2 3) (4 5 6)) (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
  (is (= #2A((1 (3 4)) (5 #\c)) (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c)))))

  (is (not (= #2A((1 2) (3 4)) #2A((1 1) (3 4)))))
  (is (not (= #2A((1 2) (3 4)) #(1 2 3 4)))))

(test nd-array-/=
  "Test `/=` on multi-dimensional arrays"

  (is (not (/= #2A((1 2 3) (4 5 6)) (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))))
  (is (not (/= #2A((1 (3 4)) (5 #\c)) (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c))))))

  (is (/= #2A((1 2) (3 4)) #2A((1 1) (3 4))))
  (is (/= #2A((1 2) (3 4)) #(1 2 3 4))))


;;; Strings

(test string-=
  "Test `=` on strings"

  (is (= "Hello" "Hello"))
  (is (= "World" (string '|World|)))
  (is (= "AAA" (make-string 3 :initial-element #\A)))
  (is (= "abc" (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c))))
  (is (= "hello" (vector #\h #\e #\l #\l #\o)))

  (is (not (= "hello" "Hello")))
  (is (not (= "world" "worlds")))
  (is (not (= "aaa" (make-string 3 :initial-element #\A))))
  (is (not (= "cba" (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c))))))

(test random-string-=
  "Test `=` on random strings"

  (for-all ((s1 (gen-string))
            (s2 (gen-string)))

    (is (not
         (xor (string= s1 s2)
              (= s1 s2))))))

(test string-/=
  "Test `/=` on strings"

  (is (not (/= "Hello" "Hello")))
  (is (not (/= "World" (string '|World|))))
  (is (not (/= "AAA" (make-string 3 :initial-element #\A))))
  (is (not (/= "hello" (vector #\h #\e #\l #\l #\o))))
  (is (not (/= "abc" (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c)))))
  (is (not (/= "hello" (vector #\h #\e #\l #\l #\o))))

  (is (/= "hello" "Hello"))
  (is (/= "world" "worlds"))
  (is (/= "aaa" (make-string 3 :initial-element #\A)))
  (is (/= "cba" (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c)))))

(test random-string-/=
  "Test `/=` on random strings"

  (for-all ((s1 (gen-string))
            (s2 (gen-string)))

    (is (not
         (xor (string/= s1 s2)
              (/= s1 s2))))))

;;; Hash-Tables

(test hash-table-=
  "Test `=` on hash-tables"

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
  "Test `=` on symbols"

  (is (= 'a 'a))
  (is (= 'sym 'sym))
  (is (= '|a symbol| '|a symbol|))
  (is (= :key :key))

  (is (not (= 'a 'b)))
  (is (not (= 'sym 'syms)))
  (is (not (= :key1 :key2)))
  (is (not (= 'a :a)))
  (is (not (= 'a '#:a))))

(test symbol-/=
  "Test `/=` on symbols"

  (is (not (/= 'a 'a)))
  (is (not (/= 'sym 'sym)))
  (is (not (/= '|a symbol| '|a symbol|)))
  (is (not (/= :key :key)))

  (is (/= 'a 'b))
  (is (/= 'sym 'syms))
  (is (/= :key1 :key2))
  (is (/= 'a :a))
  (is (/= 'a '#:a)))


;;; Different Types
(test different-types-=
  "Test `=` on non-compatible types"

  (is (not (= 1 'x)))
  (is (not (= 1 #\1)))
  (is (not (= 1 "1" #\1)))

  (is (not (= #\a 'a)))
  (is (not (= #\a "a")))

  (is (not (= '(1 2 3) '(1 2 . 3))))
  (is (not (= "hello" '|hello|))))

(test different-types-/=
  "Test `/=` on non-compatible types"

  (is (/= 1 "1" #\1))

  (is (/= #\a 'a))
  (is (/= #\a "a"))
  (is (/= #\a 'a "a")))
