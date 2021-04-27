;;; Arithmetic operator tests

(defpackage #:polymorph.maths/test.arithmetic
  (:use #:cl #:alexandria #:fiveam #:polymorph.maths/test)
  (:shadowing-import-from
   #:polymorph.maths
   #:< #:<= #:> #:>=
   #:+ #:- #:* #:/)

  (:import-from #:adhoc-polymorphic-functions
                #:no-applicable-polymorph))

(in-package #:polymorph.maths/test.arithmetic)

;;; Test suite definition

(def-suite arithmetic
    :description "Test arithmetic operators"
    :in polymorph.maths)

(in-suite arithmetic)

;;; Numbers

(test number-+
  "Test `+` on numbers."

  (is (= (+ 1 2) 3))
  (is (= (+) 0))
  (is (= (+ 2) 2))
  (is (= (+ 1 2 3 4) 10)))

(test number--
  "Test `-` on numbers."

  (is (= (- 3 4) -1))
  (is (= (- 3) -3))
  (is (= (- 4) -4))
  (is (= (- 5 4 3) -2)))

(test number-*
  "Test `*` on numbers."

  (is (= (* 2 4) 8))
  (is (= (*) 1))
  (is (= (* 3) 3))
  (is (= (* 2 3 4 5) 120)))

(test number-/
  "Test `/` on numbers."

  (is (= (/ 6 3) 2))
  (is (= (/ 5 4) 5/4))
  (is (= (/ 5) 1/5))
  (is (= (/ 6 3 2) 1)))

;;;; Random Input

(test random-number-+
  "Test `+` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (+ a b) (cl:+ a b)))
    (is (= (+ a b) (+ b a)))))

(test random-number--
  "Test `-` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (- a b) (cl:- a b)))
    (is (= (- a b) (- (- b a))))))

(test random-number-*
  "Test `*` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (* a b) (cl:* a b)))
    (is (= (* a b) (* b a)))))

(test random-number-/
  "Test `/` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (/ a b) (cl:/ a b)))))


;;; Characters

(test character-+
  "Test `+` on characters."

  (is (char=
       (+ #\a #\x)
       (code-char
        (cl:+ (char-code #\a)
              (char-code #\x)))))

  (is (char= (+ #\z) #\z))

  (is (char=
       (+ #\a #\5 #\t)
       (code-char
        (cl:+ (char-code #\a)
              (char-code #\5)
              (char-code #\t)))))

  ;; Unicode

  (is (char=
       (+ (code-char #x95)
          (code-char #x101))
       (code-char
        (cl:+ #x95 #x101)))))

(test character--
  "Test `-` on characters."

  (is (char=
       (- #\x #\a)
       (code-char
        (cl:- (char-code #\x)
              (char-code #\a)))))

  (is (char=
       (- #\t #\5 (code-char 1))
       (code-char
        (cl:- (char-code #\t)
              (char-code #\5)
              1))))

  ;; Unicode

  (is (char=
       (- (code-char #x101)
          (code-char #x95))
       (code-char
        (cl:- #x101 #x95)))))


;;; Different Types

(test different-types-+
  "Test that `+` signals condition on arguments of different types."

  (signals no-applicable-polymorph (+ 1 'x))
  (signals no-applicable-polymorph (+ 1 #\c))
  (signals no-applicable-polymorph (+ "a" "b")))

(test different-types--
  "Test that `-` signals condition on arguments of different types."

  (signals no-applicable-polymorph (- 3 "z"))
  (signals no-applicable-polymorph (- 3 #\c))
  (signals no-applicable-polymorph (- "a" "b"))
  (signals no-applicable-polymorph (- 'x)))

(test different-types-*
  "Test that `*` signals condition on arguments of different types."

  (signals no-applicable-polymorph (* 4 #\a))
  (signals no-applicable-polymorph (* 10 'x10))
  (signals no-applicable-polymorph (* "5" "6")))

(test different-types-/
  "Test that `/` signals condition on arguments of different types."

  (signals no-applicable-polymorph (/ 'a 'b))
  (signals no-applicable-polymorph (/ 3 #\2))
  (signals no-applicable-polymorph (/ "a" "b")))
