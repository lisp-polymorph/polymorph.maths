;;; Arithmetic operator tests

(defpackage #:polymorph.maths/test.arithmetic
  (:use #:cl #:alexandria #:fiveam #:polymorph.maths/test)
  (:shadowing-import-from
   #:polymorph.maths
   #:< #:<= #:> #:>=
   #:+ #:- #:* #:/)

  (:import-from #:polymorphic-functions
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

  (is (= 3 (+ 1 2)))
  (is (= 0 (+)))
  (is (= 2 (+ 2)))
  (is (= 10 (+ 1 2 3 4))))

(test number--
  "Test `-` on numbers."

  (is (= -1 (- 3 4)))
  (is (= -3 (- 3)))
  (is (= -4 (- 4)))
  (is (= -2 (- 5 4 3))))

(test number-*
  "Test `*` on numbers."

  (is (= 8 (* 2 4)))
  (is (= 1 (*)))
  (is (= 3 (* 3)))
  (is (= 120 (* 2 3 4 5))))

(test number-/
  "Test `/` on numbers."

  (is (= 2 (/ 6 3)))
  (is (= 5/4 (/ 5 4)))
  (is (= 1/5 (/ 5)))
  (is (= 1 (/ 6 3 2))))

;;;; Random Input

(test random-number-+
  "Test `+` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (cl:+ a b) (+ a b)))
    (is (= (+ a b) (+ b a)))))

(test random-number--
  "Test `-` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (cl:- a b) (- a b)))
    (is (= (- a b) (- (- b a))))))

(test random-number-*
  "Test `*` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (cl:* a b) (* a b)))
    (is (= (* a b) (* b a)))))

(test random-number-/
  "Test `/` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (cl:/ a b) (/ a b)))))


;;; Characters

(test character-+
  "Test `+` on characters."

  (is (char=
       (code-char
        (cl:+ (char-code #\a)
              (char-code #\x)))

       (+ #\a #\x)))

  (is (char= #\z (+ #\z)))

  (is (char=
       (code-char
        (cl:+ (char-code #\a)
              (char-code #\5)
              (char-code #\t)))

       (+ #\a #\5 #\t)))

  (is (char=
       (code-char
        (cl:+ (char-code #\x) (char-code #\z)))

       (+ #\z (char (make-string 5 :initial-element #\x) 3))))

  (is (char=
       (code-char
        (cl:+ (char-code #\a) (char-code #\b)))

       (+ #\a (aref (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c)) 1))))

  ;; Unicode

  (is (char=
       (code-char
        (cl:+ #x95 #x101))

       (+ (code-char #x95)
          (code-char #x101)))))

(test character--
  "Test `-` on characters."

  (is (char=
       (code-char
        (cl:- (char-code #\x)
              (char-code #\a)))

       (- #\x #\a)))

  (is (char=
       (code-char
        (cl:- (char-code #\t)
              (char-code #\5)
              1))

       (- #\t #\5 (code-char 1))))

  (is (char=
       (code-char
        (cl:- (char-code #\z) (char-code #\x)))

       (- #\z (char (make-string 5 :initial-element #\x) 3))))

  (is (char=
       (code-char
        (cl:- (char-code #\b) (char-code #\a)))

       (- (aref (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c)) 1) #\a)))

  ;; Unicode

  (is (char=
       (code-char
        (cl:- #x101 #x95))

       (- (code-char #x101)
          (code-char #x95)))))


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
