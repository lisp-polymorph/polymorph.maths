;;; Ordering comparison unit tests

(defpackage #:polymorph.maths/test.comparison
  (:use #:cl #:alexandria #:fiveam #:polymorph.maths/test)
  (:shadowing-import-from
   #:polymorph.maths
    #:= #:/=
    #:< #:<= #:> #:>=
    #:+ #:- #:* #:/
    #:min #:max)

  (:import-from #:adhoc-polymorphic-functions
                #:no-applicable-polymorph))

(in-package #:polymorph.maths/test.equality)

;;; Test suite definition

(def-suite comparison
    :description "Order comparison tests"
    :in polymorph.maths)

(in-suite comparison)


;;; Tests

;;;; Numbers

(test number-<
  "Test `<` on numbers"

  (is (< 1 2))
  (is (< 3.11 10))
  (is (< -1 0.5))

  (is (< 1))
  (is (< -1))
  (is (< 3 4))
  (is (< 3 4 5))
  (is (< 2 3 4 5 6))

  (is (not (< 5 2)))
  (is (not (< 10 2.4)))
  (is (not (< 0.3 -3)))
  (is (not (< 2 3 5 4 6))))

(test number->
  "Test `>` on numbers"

  (is (> 9 3))
  (is (> 12 5.3))
  (is (> 1/3 -4))

  (is (> 2))
  (is (> -3))
  (is (> 5 3))
  (is (> 5 4 3))
  (is (> 5 4 3 2 1))

  (is (not (> 3 7)))
  (is (not (> 2.6 10)))
  (is (not (> -8 1/6)))
  (is (not (> 5 4 1 2 3))))

(test number-<=
  "Test `<=` on numbers"

  (is (<= 2 7))
  (is (<= 7 7))
  (is (<= 1))
  (is (<= -1))
  (is (<= 1 1))
  (is (<= 1 2))
  (is (<= 1 2 3))
  (is (<= 1 2 3 4))

  (is (not (<= 9 8)))
  (is (not (<= 1 2 4 3))))

(test number->=
  "Test `>=` on numbers"

  (is (>= 10 5))
  (is (>= 10 10))
  (is (>= 4))
  (is (>= -4))
  (is (>= 2 2))
  (is (>= 3 2))
  (is (>= 3 2 1))
  (is (>= 3 3 2 1))

  (is (not (>= 0 90)))
  (is (not (>= 3 4 2 1))))

(test number-min
  "Test MIN on numbers"

  (is-every cl:=
    (0 (min 1 3 5 0 2))
    (1.5 (min 1.5 10 34/2 100.12))
    (1 (min 1))))

(test number-max
  "Test MAX on numbers"

  (is-every cl:=
    (5 (max 1 3 5 0 2))
    (200 (max 1.5 10 34/2 100.12 200))
    (4 (max 4))))


;;;; Random Numbers

(test random-number-<
  "Test `<` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:< a b)
            (< a b)))))

(test random-number->
  "Test `>` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:> a b)
            (> a b)))))

(test random-number-<=
  "Test `<=` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:<= a b)
            (<= a b)))))

(test random-number->=
  "Test `>=` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (eq (cl:>= a b)
            (>= a b)))))

(test random-number-min
  "Test MIN on random numbers"

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (cl:= (cl:min a b)
              (min a b)))))

(test random-number-max
  "Test MAX on random numbers"

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (cl:= (cl:max a b)
              (max a b)))))


;;;; Characters

(test character-<
  "Test `<` on characters"

  (is (< #\a #\b))
  (is (< #\1 #\7))
  (is (< #\a #\b #\c))

  (is (not (< #\Z #\T)))
  (is (not (< #\6 #\5))))

(test character->
  "Test `>` on characters"

  (is (> #\x #\d))
  (is (> #\4 #\1))
  (is (> #\3 #\2 #\1))

  (is (not (> #\A #\F)))
  (is (not (> #\0 #\5))))

(test character-<=
  "Test `<=` on characters"

  (is (<= #\a #\z))
  (is (<= #\c #\c))
  (is (not (<= #\x #\f))))

(test character->=
  "Test `>=` on characters"

  (is (>= #\x #\f))
  (is (>= #\r #\r))
  (is (not (>= #\b #\f))))

(test character-min
  "Test MIN on characters"

  (is-every char=
    (#\a (min #\a #\b #\z #\d))
    (#\x (min #\z #\x #\y))
    (#\c (min #\c))))

(test character-max
  "Test MAX on characters"

  (is-every char=
    (#\x (max #\a #\b #\x #\d))
    (#\z (max #\z #\x #\y))
    (#\c (max #\c))))

;;;; Random Characters

(test random-character-<
  "Test `<` on random characters."

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char< a b)
            (< a b)))))

(test random-character->
  "Test `>` on random characters."

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char> a b)
            (> a b)))))

(test random-character-<=
  "Test `<=` on random character."

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char<= a b)
            (<= a b)))))

(test random-character->=
  "Test `>=` on random character."

  (for-all ((a (gen-character))
            (b (gen-character)))

    (is (eq (cl:char>= a b)
            (>= a b)))))


;;;; Strings

(test string-<
  "Test `<` on strings"

  (is (< "aaa" "aab"))
  (is (< "hello" "hello world"))
  (is (< "hello1" "hello2"))

  (is (not (< "aax" "aaa")))
  (is (not (< "hello world" "hello")))
  (is (not (< "hello2" "hello1"))))

(test string->
  "Test `>` on strings"

  (is (> "aax" "aaa"))
  (is (> "hello world" "hello"))
  (is (> "hello3" "hello1"))

  (is (not (> "aaa" "aab")))
  (is (not (> "hello" "hello world")))
  (is (not (> "hello1" "hello2"))))

(test string-<=
  "Test `<=` on strings"

  (is (<= "aaa" "aab"))
  (is (<= "aaa" "aaa"))
  (is (not (<= "aab" "aaa"))))

(test string->=
  "Test `>=` on strings"

  (is (>= "aab" "aaa"))
  (is (>= "aaa" "aaa"))
  (is (not (>= "aaa" "aab"))))

(test string-min
  "Test MIN on strings"

  (is-every string=
    ("abc" (min "def" "abc" "xyz"))
    ("hello" (min "hello" "hello world"))
    ("bye" (min "bye" "hello"))
    ("string" (min "string"))))

(test string-max
  "Test MAX on strings"

  (is-every string=
    ("xyz" (max "def" "abc" "xyz"))
    ("hello world" (max "hello" "hello world"))
    ("hello" (max "bye" "hello"))
    ("string" (max "string"))))


;;;; Random Strings

(test random-string-<
  "Test `<` on random strings."

  (for-all ((a (gen-string))
            (b (gen-string)))

    (is (not
         (xor (string< a b)
              (< a b))))))

(test random-string->
  "Test `>` on random strings."

  (for-all ((a (gen-string))
            (b (gen-string)))

    (is (not
         (xor (string> a b)
              (> a b))))))

(test random-string-<
  "Test `<=` on random strings."

  (for-all ((a (gen-string))
            (b (gen-string)))

    (is (not
         (xor (string<= a b)
              (<= a b))))))

(test random-string->=
  "Test `>=` on random strings."

  (for-all ((a (gen-string))
            (b (gen-string)))

    (is (not
         (xor (string>= a b)
              (>= a b))))))
