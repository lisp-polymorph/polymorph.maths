;;; Arithmetic operator tests

(defpackage #:polymorph.maths/test.arithmetic
  (:use #:cl #:alexandria #:fiveam #:polymorph.maths/test)
  (:shadowing-import-from
   #:polymorph.maths
   #:< #:<= #:> #:>=
   #:+ #:- #:* #:/
   #:incf #:decf #:multf #:divf)

  (:import-from #:polymorphic-functions
                #:no-applicable-polymorph))

(in-package #:polymorph.maths/test.arithmetic)

;;; Test suite definition

(def-suite arithmetic
    :description "Test arithmetic operators"
    :in polymorph.maths)

(in-suite arithmetic)

;;; Numbers

(test-optimize number-+
  "Test `+` on numbers."

  (is (= 3 (+ 1 2)))
  (is (= 0 (+)))
  (is (= 2 (+ 2)))
  (is (= 10 (+ 1 2 3 4))))

(test-optimize number--
  "Test `-` on numbers."

  (is (= -1 (- 3 4)))
  (is (= -3 (- 3)))
  (is (= -4 (- 4)))
  (is (= -2 (- 5 4 3))))

(test-optimize number-*
  "Test `*` on numbers."

  (is (= 8 (* 2 4)))
  (is (= 1 (*)))
  (is (= 3 (* 3)))
  (is (= 120 (* 2 3 4 5))))

(test-optimize number-/
  "Test `/` on numbers."

  (is (= 2 (/ 6 3)))
  (is (= 5/4 (/ 5 4)))
  (is (= 1/5 (/ 5)))
  (is (= 1 (/ 6 3 2))))

;;;; Random Input

(test-optimize random-number-+
  "Test `+` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (cl:+ a b) (+ a b)))
    (is (= (+ a b) (+ b a)))))

(test-optimize random-number--
  "Test `-` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (cl:- a b) (- a b)))
    (is (= (- a b) (- (- b a))))))

(test-optimize random-number-*
  "Test `*` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (cl:* a b) (* a b)))
    (is (= (* a b) (* b a)))))

(test-optimize random-number-/
  "Test `/` on random numbers."

  (for-all ((a (gen-integer))
            (b (gen-integer)))

    (is (= (cl:/ a b) (/ a b)))))


;;; Characters

(test-optimize character-+
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

(test-optimize character--
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

(test-optimize different-types-+
  "Test that `+` signals condition on arguments of different types."

  (signals no-applicable-polymorph (+ 1 'x))
  (signals no-applicable-polymorph (+ 1 #\c))
  (signals no-applicable-polymorph (+ "a" "b")))

(test-optimize different-types--
  "Test that `-` signals condition on arguments of different types."

  (signals no-applicable-polymorph (- 3 "z"))
  (signals no-applicable-polymorph (- 3 #\c))
  (signals no-applicable-polymorph (- "a" "b"))
  (signals no-applicable-polymorph (- 'x)))

(test-optimize different-types-*
  "Test that `*` signals condition on arguments of different types."

  (signals no-applicable-polymorph (* 4 #\a))
  (signals no-applicable-polymorph (* 10 'x10))
  (signals no-applicable-polymorph (* "5" "6")))

(test-optimize different-types-/
  "Test that `/` signals condition on arguments of different types."

  (signals no-applicable-polymorph (/ 'a 'b))
  (signals no-applicable-polymorph (/ 3 #\2))
  (signals no-applicable-polymorph (/ "a" "b")))


;;; INCF, DECF, MULTF and DIVF

(test-optimize number-incf
  "Test INCF on numbers"

  (let ((x 2))
    (is (= 3 (incf x)))
    (is (= 3 x))

    (is (= 7 (incf x 4)))
    (is (= 7 x))))

(test-optimize char-incf
 "Test INCF on characters"
 (is
  (char= (+ #\2 #\3)
         (incf (car '(#\2)) #\3)
         #\e)))

(test-optimize number-decf
  "Test DECF on numbers"

  (let ((x 6))
    (is (= 5 (decf x)))
    (is (= 5 x))

    (is (= 2 (decf x 3)))
    (is (= 2 x))))

(test-optimize number-multf
  "Test MULTF on numbers"

  (let ((num 11))
    (is (= 11 (multf num)))
    (is (= 11 num))

    (is (= 22 (multf num 2)))
    (is (= 22 num))))

(test-optimize number-divf
  "Test DIVF on numbers"

  (let ((num 55))
    (is (= 55 (divf num)))
    (is (= 55 num))

    (is (= 11 (divf num 5)))
    (is (= 11 num))))
