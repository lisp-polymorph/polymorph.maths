;;; Ordering comparison unit tests

(defpackage #:polymorph.maths/test.comparison
  (:use #:cl #:alexandria #:fiveam #:polymorph.maths/test)
  (:shadowing-import-from
   #:polymorph.maths
   #:= #:/=
   #:< #:<= #:> #:>=
   #:+ #:- #:* #:/
   #:min #:max)

  (:import-from #:polymorphic-functions
                #:no-applicable-polymorph))

(in-package #:polymorph.maths/test.comparison)

;;; Test suite definition

(def-suite comparison
    :description "Order comparison tests"
    :in polymorph.maths)

(in-suite comparison)


;;; Tests

;;;; Numbers

(test-optimize number-<
  "Test `<` on numbers"

  (is-true (< 1 2))
  (is-true (< 3.11 10))
  (is-true (< -1 0.5))

  (is-true (< 1))
  (is-true (< -1))
  (is-true (< 3 4))
  (is-true (< 3 4 5))
  (is-true (< 2 3 4 5 6))

  (is-false (< 5 2))
  (is-false (< 10 2.4))
  (is-false (< 0.3 -3))
  (is-false (< 2 3 5 4 6)))

(test-optimize number->
  "Test `>` on numbers"

  (is-true (> 9 3))
  (is-true (> 12 5.3))
  (is-true (> 1/3 -4))

  (is-true (> 2))
  (is-true (> -3))
  (is-true (> 5 3))
  (is-true (> 5 4 3))
  (is-true (> 5 4 3 2 1))

  (is-false (> 3 7))
  (is-false (> 2.6 10))
  (is-false (> -8 1/6))
  (is-false (> 5 4 1 2 3)))

(test-optimize number-<=
  "Test `<=` on numbers"

  (is-true (<= 2 7))
  (is-true (<= 7 7))
  (is-true (<= 1))
  (is-true (<= -1))
  (is-true (<= 1 1))
  (is-true (<= 1 2))
  (is-true (<= 1 2 3))
  (is-true (<= 1 2 3 4))

  (is-false (<= 9 8))
  (is-false (<= 1 2 4 3)))

(test-optimize number->=
  "Test `>=` on numbers"

  (is-true (>= 10 5))
  (is-true (>= 10 10))
  (is-true (>= 4))
  (is-true (>= -4))
  (is-true (>= 2 2))
  (is-true (>= 3 2))
  (is-true (>= 3 2 1))
  (is-true (>= 3 3 2 1))

  (is-false (>= 0 90))
  (is-false (>= 3 4 2 1)))

(test-optimize number-min
  "Test MIN on numbers"

  (is-every cl:=
    (0 (min 1 3 5 0 2))
    (1.5 (min 1.5 10 34/2 100.12))
    (1 (min 1))))

(test-optimize number-max
  "Test MAX on numbers"

  (is-every cl:=
    (5 (max 1 3 5 0 2))
    (200 (max 1.5 10 34/2 100.12 200))
    (4 (max 4))))


;;;; Random Numbers

(test-optimize random-number-<
  "Test `<` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:< a b)
            (< a b)))))

(test-optimize random-number->
  "Test `>` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:> a b)
            (> a b)))))

(test-optimize random-number-<=
  "Test `<=` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:<= a b)
            (<= a b)))))

(test-optimize random-number->=
  "Test `>=` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:>= a b)
            (>= a b)))))

(test-optimize random-number-min
  "Test MIN on random numbers"

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (cl:= (cl:min a b)
              (min a b)))))

(test-optimize random-number-max
  "Test MAX on random numbers"

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (cl:= (cl:max a b)
              (max a b)))))


;;;; Characters

(test-optimize character-<
  "Test `<` on characters"

  (is-true (< #\a #\b))
  (is-true (< #\1 #\7))
  (is-true (< (char (make-string 10 :initial-element #\d) 4) #\x))
  (is-true (< (aref (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d)) 2)
              #\d))
  (is-true (< #\a #\b #\c))

  (is-false (< #\Z #\T))
  (is-false (< #\6 #\5))
  (is-false (< (char (make-string 10 :initial-element #\d) 4) #\a))
  (is-false (< (aref (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d)) 3)
               #\d))
  (is-false (< #\c #\d #\a)))

(test-optimize character->
  "Test `>` on characters"

  (is-true (> #\x #\d))
  (is-true (> #\4 #\1))
  (is-true (> (char (make-string 10 :initial-element #\d) 4) #\c))
  (is-true (> (aref (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d)) 2)
              #\b))
  (is-true (> #\3 #\2 #\1))

  (is-false (> #\A #\F))
  (is-false (> #\0 #\5))
  (is-false (> (char (make-string 10 :initial-element #\d) 4) #\x))
  (is-false (> (aref (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d)) 2)
               #\c))
  (is-false (> #\d #\c #\x)))

(test-optimize character-<=
  "Test `<=` on characters"

  (is-true (<= #\a #\z))
  (is-true (<= #\c #\c))
  (is-true (<= (char (make-string 3 :initial-element #\e) 0) #\f))
  (is-true (<= (aref (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d)) 2)
               #\c))

  (is-false (<= #\x #\f))
  (is-false (<= (char (make-string 3 :initial-element #\e) 0) #\c))
  (is-false (<= (aref (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d)) 3)
                #\c))
  (is-false (<= #\x #\x #\g)))

(test-optimize character->=
  "Test `>=` on characters"

  (is-true (>= #\x #\f))
  (is-true (>= #\r #\r))
  (is-true (>= (char (make-string 10 :initial-element #\d) 4) #\c))
  (is-true (>= (aref (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d)) 1)
               #\b))

  (is-false (>= #\b #\f))
  (is-false (>= (char (make-string 10 :initial-element #\d) 4) #\t))
  (is-false (>= (aref (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d)) 1)
                #\c))
  (is-false (>= #\x #\g #\h)))

(test-optimize character-min
  "Test MIN on characters"

  (is-every char=
    (#\a (min #\a #\b #\z #\d))
    (#\x (min #\z #\x #\y))
    (#\c (min #\c))
    (#\b (min #\x (char (make-string 4 :initial-element #\b) 2)))
    (#\c (min #\d (aref (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c)) 2)))))

(test-optimize character-max
  "Test MAX on characters"

  (is-every char=
    (#\x (max #\a #\b #\x #\d))
    (#\z (max #\z #\x #\y))
    (#\c (max #\c))
    (#\x (max #\x (char (make-string 4 :initial-element #\b) 2)))
    (#\f (max #\d (aref (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\f)) 2)))))

;;;; Random Characters

(test-optimize random-character-<
  "Test `<` on random characters."

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char< a b)
            (< a b)))))

(test-optimize random-character->
  "Test `>` on random characters."

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char> a b)
            (> a b)))))

(test-optimize random-character-<=
  "Test `<=` on random character."

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char<= a b)
            (<= a b)))))

(test-optimize random-character->=
  "Test `>=` on random character."

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char>= a b)
            (>= a b)))))


;;;; Strings

(test-optimize string-<
  "Test `<` on strings"

  (is-true (< "aaa" "aab"))
  (is-true (< "hello" "hello world"))
  (is-true (< "hello1" "hello2"))
  (is-true (< (make-string 3 :initial-element #\e) "eeef"))
  (is-true (< (make-array 3 :element-type 'character :initial-contents '(#\t #\u #\v)) "tux"))

  (is-false (< "aax" "aaa"))
  (is-false (< "hello world" "hello"))
  (is-false (< "hello2" "hello1"))
  (is-false (< (make-string 3 :initial-element #\f) "eeef"))
  (is-false (< (make-array 3 :element-type 'character :initial-contents '(#\t #\u #\v)) "tuu")))

(test-optimize string->
  "Test `>` on strings"

  (is-true (> "aax" "aaa"))
  (is-true (> "hello world" "hello"))
  (is-true (> "hello3" "hello1"))
  (is-true (> "eeef" (make-string 3 :initial-element #\e)))
  (is-true (> "tux" (make-array 3 :element-type 'character :initial-contents '(#\t #\u #\v))))

  (is-false (> "aaa" "aab"))
  (is-false (> "hello" "hello world"))
  (is-false (> "hello1" "hello2"))
  (is-false (> "eeef" (make-string 3 :initial-element #\f)))
  (is-false (> "tuu" (make-array 3 :element-type 'character :initial-contents '(#\t #\u #\v)))))

(test-optimize string-<=
  "Test `<=` on strings"

  (is-true (<= "aaa" "aab"))
  (is-true (<= "aaa" "aaa"))
  (is-true (<= "abc" "abcd" "abce"))
  (is-true (<= (make-string 3 :initial-element #\e) "eee"))
  (is-true (<= (make-array 3 :element-type 'character :initial-contents '(#\t #\u #\v)) "tux"))

  (is-false (<= "aab" "aaa"))
  (is-false (<= "abc" "abcd" "abb"))
  (is-false (<= (make-string 3 :initial-element #\e) "aaa"))
  (is-false (<= (make-array 3 :element-type 'character :initial-contents '(#\t #\u #\v)) "tuu")))

(test-optimize string->=
  "Test `>=` on strings"

  (is-true (>= "aab" "aaa"))
  (is-true (>= "aaa" "aaa"))
  (is-true (>= "eee" (make-string 3 :initial-element #\e)))
  (is-true (>= "tux" (make-array 3 :element-type 'character :initial-contents '(#\t #\u #\v))))

  (is-false (>= "aaa" "aab"))
  (is-false (>= "hello" "bye" "hello world"))
  (is-false (>= "aaa" (make-string 3 :initial-element #\e)))
  (is-false (>= "tuu" (make-array 3 :element-type 'character :initial-contents '(#\t #\u #\v)))))

(test-optimize string-min
  "Test MIN on strings"

  (is-every string=
    ("abc" (min "def" "abc" "xyz"))
    ("hello" (min "hello" "hello world"))
    ("bye" (min "bye" "hello"))
    ("string" (min "string"))
    ("aaa" (min "abc" (make-string 3 :initial-element #\a)))
    ("def" (min "def" (make-array 4 :element-type 'character :initial-contents '(#\d #\e #\f #\g))))))

(test-optimize string-max
  "Test MAX on strings"

  (is-every string=
    ("xyz" (max "def" "abc" "xyz"))
    ("hello world" (max "hello" "hello world"))
    ("hello" (max "bye" "hello"))
    ("string" (max "string"))
    ("abc" (max "abc" (make-string 3 :initial-element #\a)))
    ("defg" (max "def" (make-array 4 :element-type 'character :initial-contents '(#\d #\e #\f #\g))))))


;;;; Random Strings

(test-optimize random-string-<
  "Test `<` on random strings."

  (for-all ((a (gen-string))
            (b (gen-string)))

    (is (not
         (xor (string< a b)
              (< a b))))))

(test-optimize random-string->
  "Test `>` on random strings."

  (for-all ((a (gen-string))
            (b (gen-string)))

    (is (not
         (xor (string> a b)
              (> a b))))))

(test-optimize random-string-<
  "Test `<=` on random strings."

  (for-all ((a (gen-string))
            (b (gen-string)))

    (is (not
         (xor (string<= a b)
              (<= a b))))))

(test-optimize random-string->=
  "Test `>=` on random strings."

  (for-all ((a (gen-string))
            (b (gen-string)))

    (is (not
         (xor (string>= a b)
              (>= a b))))))
