;;; Equality comparison unit tests

(defpackage #:polymorph.maths/test.equality
  (:use #:cl #:alexandria #:fiveam #:polymorph.maths/test)
  (:shadowing-import-from
   #:polymorph.maths
    #:= #:/=
    #:< #:<= #:> #:>=
    #:+ #:- #:* #:/)

  (:import-from #:polymorphic-functions
                #:no-applicable-polymorph))

(in-package #:polymorph.maths/test.equality)

;;; Test suite definition

(def-suite equality
    :description "Equality comparison tests"
    :in polymorph.maths)

(in-suite equality)


;;; Tests

;;;; Numbers

(test-optimize number-=
  "Test `=` on numbers"

  (is-true (= 1 1))
  (is-true (= 2.0 2))
  (is-true (= 6/3 2))

  (is-false (= 1 0))
  (is-false (= 2.3 5.7))

  (is-true (= 1))
  (is-true (= 1 1.0 2/2))
  (is-false (= 1 1 3)))

(test-optimize number-/=
  "Test `/=` on numbers"

  (is-true (/= 1 0))
  (is-true (/= 2 3))
  (is-true (/= 1))
  (is-true (/= 1 1 3))
  (is-false (/= 1 1.0 2/2)))

(test-optimize random-number-=
  "Test `=` on random numbers"

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:= a b)
            (= a b)))

    (is (eq (= b a) (= a b)))))

(test-optimize random-number-/=
  "Test `/=` on random numbers"

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:/= a b)
            (/= a b)))

    (is (eq (/= b a) (/= a b)))))


;;;; Characters

(test-optimize character-=
  "Test `=` on characters"

  (is-true (= #\a #\a))
  (is-true (= #\0 #\0))
  (is-true (= #\b (char (make-string 5 :initial-element #\b) 3)))
  (is-true (= #\y (aref (make-array 3 :element-type 'character :initial-contents '(#\x #\y #\z)) 1)))

  (is-false (= #\a #\A))
  (is-false (= #\x (char (make-string 5 :initial-element #\b) 3)))
  (is-false (= #\z (aref (make-array 3 :element-type 'character :initial-contents '(#\x #\y #\z)) 1)))

  (is-true (= #\a #\a #\a))
  (is-false (= #\a #\A 'a)))

(test-optimize random-character-=
  "Test `=` on random characters"

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char= a b)
            (= a b)))

    (is (eq (= b a) (= a b)))))

(test-optimize character-/=
  "Test `/=` on characters"

  (is-true (/= #\a #\A))
  (is-true (/= #\x #\y))
  (is-true (/= #\x (char (make-string 5 :initial-element #\b) 3)))
  (is-true (/= #\z (aref (make-array 3 :element-type 'character :initial-contents '(#\x #\y #\z)) 1)))
  (is-true (/= #\d #\d #\e))

  (is-false (/= #\a #\a #\a))
  (is-false (/= #\b (char (make-string 5 :initial-element #\b) 3)))
  (is-false (/= #\y (aref (make-array 3 :element-type 'character :initial-contents '(#\x #\y #\z)) 1))))

(test-optimize random-character-/=
  "Test `/=` on random characters"

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char/= a b)
            (/= a b)))

    (is (eq (/= b a) (/= a b)))))


;;;; Conses/Lists

(test-optimize cons-=
  "Test `=` on lists and cons"

  (is-true (= '(1 2 3) (list 1.0 2 3.0)))
  (is-true (= '(a b c) (cons 'a (cons 'b (cons 'c nil)))))
  (is-true (= '(1 a #\x) (list 2/2 'a #\x)))
  (is-true (= '(((1 2) x y) #\z) (list (list (list 1 2) 'x 'y) #\z)))
  (is-true (= '("abc" "def") (list "abc" "def")))
  (is-true (= nil (cdr (list '1))))
  (is-true (= '(5 6 . 3) '(5.0 6 . 3.0)))
  (is-true (= (make-list 100) (make-list 100)))

  (is-false (= '(1 2 3) '(1 2 1)))
  (is-false (= '(1 2 3) '(1 2))))

(test-optimize cons-/=
  "Test `/=` on lists and cons"

  (is-false (/= '(1 2 3) (list 1.0 2 3.0)))
  (is-false (/= '(a b c) (cons 'a (cons 'b (cons 'c nil)))))
  (is-false (/= '(1 a #\x) (list 2/2 'a #\x)))
  (is-false (/= '(((1 2) x y) #\z) (list (list (list 1 2) 'x 'y) #\z)))
  (is-false (/= '("abc" "def") (list "abc" "def")))
  (is-false (/= nil (cdr (list '1))))
  (is-false (/= '(5 6 . 3) '(5.0 6 . 3.0)))

  (is-true (/= '(1 2 3) '(1 2 1)))
  (is-true (/= '(1 2 3) '(1 2))))


;;; Single-dimensional Arrays (Vector)

(test-optimize vector-=
  "Test `=` on single dimensional arrays (vectors)"

  (is-true (= #(1 2 3) (vector 1 2 3)))
  (is-true (= #(1 2 3) (make-array 3
                              :element-type 'number
                              :adjustable t
                              :fill-pointer t
                              :initial-contents '(1 2 3))))

  (is-true (= #(1 2 x) (vector 1.0 2 'x)))
  (is-true (= #(#(1 2) 3) (vector (vector 1.0 2.0) 3)))
  (is-true (= #((1 2) 3) (vector '(1.0 2.0) 3)))

  (is-false (= #(1 2 3) #(1 1 1)))
  (is-false (= #(1 2 3) #(1 2 3 4)))
  (is-false (= #(1 2 3) (make-array 0)))
  (is-false (= #(1 2 3) (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
  (is-false (= #(#(1 2)) #(#(2 1)))))

(test-optimize vector-/=
  "Test `/=` on single dimensional arrays (vectors)"

  (is-false (/= #(1 2 3) (vector 1 2 3)))
  (is-false (/= #(1 2 3) (make-array 3
                                :element-type 'number
                                :adjustable t
                                :fill-pointer t
                                :initial-contents '(1 2 3))))

  (is-false (/= #(1 2 x) (vector 1.0 2 'x)))
  (is-false (/= #(#(1 2) 3) (vector (vector 1.0 2.0) 3)))
  (is-false (/= #((1 2) 3) (vector '(1.0 2.0) 3)))

  (is-true (/= #(1 2 3) #(1 1 1)))
  (is-true (/= #(1 2 3) #(1 2 3 4)))
  (is-true (/= #(1 2 3) (make-array 0)))
  (is-true (/= #(1 2 3) (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
  (is-true (/= #(#(1 2)) #(#(2 1)))))


;;; Multi-dimensional Arrays

(test-optimize nd-array-=
  "Test `=` on multi-dimensional arrays"

  (is-true (= #2A((1 2 3) (4 5 6)) (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
  (is-true (= #2A((1 (3 4)) (5 #\c)) (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c)))))

  (is-false (= #2A((1 2) (3 4)) #2A((1 1) (3 4))))
  (is-false (= #2A((1 2) (3 4)) #(1 2 3 4))))

(test-optimize nd-array-/=
  "Test `/=` on multi-dimensional arrays"

  (is-false (/= #2A((1 2 3) (4 5 6)) (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
  (is-false (/= #2A((1 (3 4)) (5 #\c)) (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c)))))

  (is-true (/= #2A((1 2) (3 4)) #2A((1 1) (3 4))))
  (is-true (/= #2A((1 2) (3 4)) #(1 2 3 4))))


;;; Strings

(test-optimize string-=
  "Test `=` on strings"

  (is-true (= "Hello" "Hello"))
  (is-true (= "World" (string '|World|)))
  (is-true (= "AAA" (make-string 3 :initial-element #\A)))
  (is-true (= "abc" (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c))))
  (is-true (= "hello" (vector #\h #\e #\l #\l #\o)))

  (is-false (= "hello" "Hello"))
  (is-false (= "world" "worlds"))
  (is-false (= "aaa" (make-string 3 :initial-element #\A)))
  (is-false (= "cba" (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c)))))

(test-optimize random-string-=
  "Test `=` on random strings"

  (for-all ((s1 (gen-string))
            (s2 (gen-string)))

    (is (not
         (xor (string= s1 s2)
              (= s1 s2))))))

(test-optimize string-/=
  "Test `/=` on strings"

  (is-false (/= "Hello" "Hello"))
  (is-false (/= "World" (string '|World|)))
  (is-false (/= "AAA" (make-string 3 :initial-element #\A)))
  (is-false (/= "hello" (vector #\h #\e #\l #\l #\o)))
  (is-false (/= "abc" (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c))))
  (is-false (/= "hello" (vector #\h #\e #\l #\l #\o)))

  (is-true (/= "hello" "Hello"))
  (is-true (/= "world" "worlds"))
  (is-true (/= "aaa" (make-string 3 :initial-element #\A)))
  (is-true (/= "cba" (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c)))))

(test-optimize random-string-/=
  "Test `/=` on random strings"

  (for-all ((s1 (gen-string))
            (s2 (gen-string)))

    (is (not
         (xor (string/= s1 s2)
              (/= s1 s2))))))

;;; Hash-Tables

(test-optimize hash-table-=
  "Test `=` on hash-tables"

  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash 'x table) 1)
    (setf (gethash 'y table) 'z)
    (setf (gethash "hello" table) "world")
    (setf (gethash '(1 2 3) table) #\z)

    (is-true (= table
           (alist-hash-table
            '((x . 1) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-true (= table
           (alist-hash-table
            '((x . 1) (y . z) ("HELLO" . "world") ((1 2 3) . #\z)) :test #'equalp)))

    (is-false
         (= table
            (alist-hash-table
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-false
         (= table
            (alist-hash-table
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-false
         (= table
            (alist-hash-table
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z) ("x" . "z")))))))


;;; Symbols

(test-optimize symbol-=
  "Test `=` on symbols"

  (is-true (= 'a 'a))
  (is-true (= 'sym 'sym))
  (is-true (= '|a symbol| '|a symbol|))
  (is-true (= :key :key))

  (is-false (= 'a 'b))
  (is-false (= 'sym 'syms))
  (is-false (= :key1 :key2))
  (is-false (= 'a :a))
  (is-false (= 'a '#:a)))

(test-optimize symbol-/=
  "Test `/=` on symbols"

  (is-false (/= 'a 'a))
  (is-false (/= 'sym 'sym))
  (is-false (/= '|a symbol| '|a symbol|))
  (is-false (/= :key :key))

  (is-true (/= 'a 'b))
  (is-true (/= 'sym 'syms))
  (is-true (/= :key1 :key2))
  (is-true (/= 'a :a))
  (is-true (/= 'a '#:a)))


;;; Different Types
#||
(test-optimize different-types-=
  "Test `=` on non-compatible types"

  (is (not (= 1 'x)))
  (is (not (= 1 #\1)))
  (is (not (= 1 "1" #\1)))

  (is (not (= #\a 'a)))
  (is (not (= #\a "a")))

  (is (not (= '(1 2 3) '(1 2 . 3))))
  (is (not (= "hello" '|hello|))))

(test-optimize different-types-/=
  "Test `/=` on non-compatible types"

  (is (/= 1 "1" #\1))

  (is (/= #\a 'a))
  (is (/= #\a "a"))
  (is (/= #\a 'a "a")))
||#
