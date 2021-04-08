;;;; polymorph.maths.lisp

(in-package #:polymorph.maths)


(defun %form-type (form &optional env)
  (adhoc-polymorphic-functions::form-type form env))

(deftype ind () `(integer 0 #.array-dimension-limit))

(defun %check-container-elem-applicable (elem-type1 elem-type2 fn &optional env)
  (let ((all (adhoc-polymorphic-functions::polymorphic-function-type-lists fn)))
    (unless (find (list elem-type1 elem-type2)
                  all
                  :test (lambda (given existing)
                          (destructuring-bind (gfst gsnd) given
                            (destructuring-bind (efst esnd) existing
                              (and (subtypep gfst efst env)
                                 (subtypep gsnd esnd env))))))
      (error "Types ~s and ~s are incompatbile in terms of function ~s~%"
               elem-type1 elem-type2 fn))))





;; Equality
(define-polymorphic-function = (first second) :overwrite t
  :documentation "Return T if all of its arguments are , NIL otherwise.")

(define-polymorphic-function /= (first second) :overwrite t
  :documentation "Return T if all of its arguments are , NIL otherwise.")

(defpolymorph = ((first number) (second number)) (values boolean &optional)
  (cl:= first second))


(defpolymorph = ((first symbol) (second symbol)) (values boolean &optional)
  (eql first second))

(defpolymorph = ((first character) (second character)) (values boolean &optional)
  (char= first second))

(defpolymorph = ((first string) (second string)) (values boolean &optional)
  (string= first second))


(defpolymorph (= :inline t) ((first cons) (second cons)) (values boolean &optional)
              (and (= (car first) (car second))
                 (= (cdr first) (cdr second))))



(defpolymorph (= :inline t) ((first array) (second array)) (values boolean &optional)
  (let ((s1 (array-total-size first))
        (s2 (array-total-size second)))
    (and (cl:= s1 s2)
       (loop :for i :below s1
             :always (= (row-major-aref first i)
                        (row-major-aref second i))))))

(defpolymorph-compiler-macro = (array array) (first second &environment env)
  (let* ((type1 (%form-type first env))
         (elt1  (cm:array-type-element-type type1))
         (dim1  (cm:array-type-dimensions type1))
         (type2 (%form-type second env))
         (elt2  (cm:array-type-element-type type2))
         (dim2  (cm:array-type-dimensions type2))
         (s1 (gensym))
         (s2 (gensym))
         (i (gensym)))
    (%check-container-elem-applicable elt1 elt2 #'=)               ;;TODO can it bug out on arrays of arrays?
    (unless (equalp dim1 dim2)
      (warn "Arrays dimensions are not known to be compatbile"))
    (once-only (first second)
      `(let ((,s1 (array-total-size ,first))
             (,s2 (array-total-size ,second)))
         (and (cl:= ,s1 ,s2)
            (loop :for ,i :below ,s1
                  :always (= (the ,elt1 (row-major-aref ,first ,i))
                             (the ,elt2 (row-major-aref ,second ,i)))))))))


(defpolymorph = ((first hash-table) (second hash-table)) (values boolean &optional)
  (and ;;(eq (hash-table-test first) (hash-table-test second))
     ;;TODO Do I test this? What else do I (not) test?
   (cl:= (hash-table-size first) (hash-table-size second))
   (cl:= (hash-table-count first) (hash-table-count second))
   (loop :for key1 :being :the :hash-keys :in first
           :using (hash-value v1)
         :always (multiple-value-bind (v2 exists) (gethash key1 second)
                   (and exists
                      (= v1 v2))))))


(defpolymorph (= :inline t) ((first structure-object) (second structure-object))
    (values boolean &optional)
  (let* ((type1        (type-of first))
         (type2        (type-of second)))
   (and (eql type1 type2)
      (loop :for slot :in (mop:class-slots (find-class type1))
            :for name := (mop:slot-definition-name slot)
            :always (= (slot-value first name) (slot-value second name))))))

(defpolymorph-compiler-macro = (structure-object structure-object) (first second &environment env)
  (let* ((type1        (%form-type first env))
         (type2        (%form-type second env)))
    (unless (eql type1 type2)
      (error "Structures of different types are never ="))
   `(and
     ,@(loop :for slot :in (mop:class-slots (find-class type1 t env))
             :for name := (mop:slot-definition-name slot)
             :for type := (mop:slot-definition-type slot)
             :collect `(= (the ,type (slot-value ,first ',name))
                          (the ,type (slot-value ,second ',name)))))))



(defpolymorph (/= :inline t) ((first t) (second t)) (values boolean &optional)
  (not (= first second)))






;; Inequality
(define-polymorphic-function < (first second) :overwrite t
  :documentation "Return T if its arguments are in strictly increasing order, NIL otherwise.")
(define-polymorphic-function <= (first second) :overwrite t
  :documentation "Return T if arguments are in strictly non-decreasing order, NIL otherwise.")
(define-polymorphic-function > (first second) :overwrite t
  :documentation "Return T if its arguments are in strictly increasing order, NIL otherwise.")
(define-polymorphic-function >= (first second) :overwrite t
  :documentation "Return T if arguments are in strictly non-decreasing order, NIL otherwise.")

(defpolymorph < ((first real) (second real)) (values boolean &optional)
              (cL:< first second))

(defpolymorph <= ((first real) (second real)) (values boolean &optional)
              (cl:<= first second))

(defpolymorph < ((first character) (second character)) (values boolean &optional)
              (char< first second))

(defpolymorph <= ((first character) (second character)) (values boolean &optional)
              (char<= first second))

(defpolymorph < ((first string) (second string)) (values boolean &optional)
              (string< first second))

(defpolymorph <= ((first string) (second string)) (values boolean &optional)
              (string<= first second))



(defpolymorph > ((first t) (second t)) (values boolean &optional)
  (not (<= first second)))
(defpolymorph >= ((first t) (second t)) (values boolean &optional)
  (not (< first second)))


;; Arithmetics
(define-polymorphic-function + (&rest xs) :overwrite t)

(defpolymorph + ((a t)) t
  a)

(defpolymorph + ((first number) (second number)) number
  (cl:+ first second))

(defpolymorph + ((first character) (second character)) character
  (code-char (cl:+ (char-code first) (char-code second))))

(defpolymorph (+ :inline t) ((first t) (second t) (third t) &rest xs) t
  (cl:reduce #'+ xs :initial-value (+ (+ first second) third)))


(defpolymorph-compiler-macro + (t t t &rest) (first second third &rest xs)
  (labels ((gen+ (ls done)
             (if ls
                 (gen+ (cdr ls) `(+ ,done ,(car ls)))
                 done)))
    `(the ,(cm:form-type first) ,(gen+ xs `(+ (+ ,first ,second) ,third)))))


(define-polymorphic-function - (x &rest xs) :overwrite t)

(defpolymorph - ((a number)) number
  (cl:- a))

(defpolymorph - ((first number) (second number)) number
  (cl:- first second))

(defpolymorph - ((first character) (second character)) character
  (code-char (cl:- (char-code first) (char-code second))))



(defpolymorph (- :inline t) ((first t) (second t) (third t) &rest xs) t
  (cl:reduce #'- xs :initial-value (- (- first second) third)))


(defpolymorph-compiler-macro - (t t t &rest) (first second third &rest xs)
  (labels ((gen- (ls done)
             (if ls
                 (gen- (cdr ls) `(- ,done ,(car ls)))
                 done)))))



(define-polymorphic-function * (&rest xs) :overwrite t)

(defpolymorph * ((a t)) t
  a)

(defpolymorph * ((first number) (second number)) number
  (cl:* first second))

(defpolymorph (* :inline t) ((first t) (second t) (third t) &rest xs) t
  (cl:reduce #'* xs :initial-value (* (* first second) third)))


(defpolymorph-compiler-macro * (t t t &rest) (first second third &rest xs)
  (labels ((gen* (ls done)
             (if ls
                 (gen* (cdr ls) `(* ,done ,(car ls)))
                 done)))
    `(the ,(cm:form-type first) ,(gen* xs `(* (* ,first ,second) ,third)))))


(define-polymorphic-function / (x &rest xs) :overwrite t)

(defpolymorph / ((a number)) number
  (cl:/ a))

(defpolymorph / ((first number) (second number)) number
  (cl:/ first second))



(defpolymorph (/ :inline t) ((first t) (second t) (third t) &rest xs) t
  (cl:reduce #'/ xs :initial-value (/ (/ first second) third)))


(defpolymorph-compiler-macro / (t t t &rest) (first second third &rest xs)
  (labels ((gen/ (ls done)
             (if ls
                 (gen/ (cdr ls) `(/ ,done ,(car ls)))
                 done)))
    `(the ,(cm:form-type first) ,(gen/ xs `(/ (/ ,first ,second) ,third)))))
