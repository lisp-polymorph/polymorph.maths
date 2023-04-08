;;;; polymorph.maths.lisp

(in-package #:polymorph.maths)

(define-symbol-macro * cl:*)
(define-symbol-macro + cl:+)

;; Equality
(define-polymorphic-function = (object &rest objects) :overwrite t
  :documentation
  "Return T if all of its arguments are equal, in a sense dictated by their type;
NIL otherwise.")

(define-polymorphic-function /= (object &rest objects) :overwrite t
  :documentation
  "Inequality is defined as negation of [=][polymorphic-function].
Meaning it is T when some of its arguments are not [=][polymorphic-function].")

(defpolymorph (= :inline t) ((first t)) (eql t)
  "Extend CL:= equality to a single argument of any type. Returns T in this case."
  (declare (ignorable first))
  t)

(defpolymorph (/= :inline t) ((first t)) (eql t)
  "Extend CL:/= inequality to a single argument of any type."
  (declare (ignorable first))
  t)

(defpolymorph (= :inline t) ((first number) (second number)) (values boolean &optional)
  (cl:= first second))

(defpolymorph (= :inline t) ((first symbol) (second symbol)) (values boolean &optional)
  (eql first second))

(defpolymorph (= :inline t) ((first character) (second character)) (values boolean &optional)
  (char= first second))

(defpolymorph (= :inline t) ((first string) (second string)) (values boolean &optional)
  (string= first second))

(defpolymorph (= :inline t) ((first cons) (second cons)) (values boolean &optional)
  (and (= (car first) (car second))
     (= (cdr first) (cdr second))))

(defpolymorph (= :inline t) ((first cons) (second null)) (values boolean &optional)
  "Non-empty and empty CONS (or NIL) are not equal."
  (declare (ignorable first second))
  nil)

(defpolymorph (= :inline t) ((first null) (second cons)) (values boolean &optional)
  "Empty lists (or NIL) and non-empty CONS are not equal."
  (declare (ignorable first second))
  nil)

(defpolymorph (= :inline t) ((first array)
                             (second array))
    (values boolean &optional)
  (let ((s1 (array-total-size first))
        (s2 (array-total-size second)))
    (and (cl:= s1 s2)
       (equal (array-dimensions first) (array-dimensions second))
       (loop :for i :of-type ind :below s1
             :always (= (row-major-aref first i)
                        (row-major-aref second i))))))

(defpolymorph-compiler-macro = (array array) (&whole form first second &environment env)
  (with-type-info (type1 (typename1 &optional (elt1 'cl:*) dim1) env) first
    (with-type-info (type2 (typename2 &optional (elt2 'cl:*) dim2) env) second
      (when-types ((typename1 array) (typename2 array)) form

        (let ((size1 (cond
                                        ;((and (listp dim1) (every (lambda (x) (constantp x env)) dim1))
                                        ; (reduce (lambda (a b) `(cl:* ,a ,b)) dim1)
                       (t `(array-total-size ,first))))
              (i (gensym))
              (dim-check ;(if (and (and (listp dim1) (every (lambda (x) (constantp x env)) dim1))
                                        ;       (and (listp dim2) (every (lambda (x) (constantp x env)) dim2))
                                        ;       (every (lambda (x) (numberp x)) dim1)
                                        ;       (every (lambda (x) (numberp x)) dim2)
                                        ;    (cl:= (reduce #'cl:* dim1) (reduce #'cl:* dim2))
               `(equal (array-dimensions ,first) (array-dimensions ,second))))
          (unless (equalp dim1 dim2)
            (warn "Arrays dimensions are not known to be compatbile"))
          (once-only (first second)
            `(and
              ,dim-check
              (loop :for ,i :of-type ind :below ,size1
                 :always (= (the ,elt1 (row-major-aref ,first ,i))
                            (the ,elt2 (row-major-aref ,second ,i)))))))))))


(defpolymorph (= :inline t) ((first (and vector (not simple-array) (not string)))
                             (second (and vector (not simple-array) (not string))))
    (values boolean &optional)
  (let ((s1 (length first))
        (s2 (length second)))
    (and (cl:= s1 s2)
       (cl:= (fill-pointer first) (fill-pointer second))
       (loop :for i :of-type ind :below s1
             :always (= (aref first i)
                        (aref second i))))))


(defpolymorph-compiler-macro = ((and vector (not simple-array) (not string))
                                (and vector (not simple-array) (not string)))
    (&whole form first second &environment env)

  (with-type-info (type1 (typename1 &optional (elt1 'cl:*)) env) first
    (with-type-info (type2 (typename2 &optional (elt2 'cl:*)) env) second
      (when-types ((typename1 array) (typename2 array)) form
        (let* ((s1 (gensym))
               (s2 (gensym))
               (i (gensym))
               (check ;(if (and (constantp dim1 env) (constantp dim2 env))
                                        ;    `(= ,dim1 ,dim2)
                `(= ,s1 ,s2)))
          (once-only (first second)
            `(let ((,s1 (length ,first))
                   (,s2 (length ,second)))
               (declare (ignorable ,s2))
               (and ,check
                    (cl:= (fill-pointer ,first) (fill-pointer ,second))
                    (loop :for ,i :of-type ind :below ,s1
                       :always (= (the ,elt1 (aref ,first ,i))
                                  (the ,elt2 (aref ,second ,i))))))))))))



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

#||
(defpolymorph (= :inline t) ((first structure-object) (second structure-object))
    (values boolean &optional)
  (equalp first second))
||#
#||
;; FIXME This is bad as well
;; It should just be equalp
(defpolymorph (= :inline t) ((first structure-object) (second structure-object))
    (values boolean &optional)
  (let* ((type1        (type-of first))
         (type2        (type-of second)))
   (and (eql type1 type2)
      (loop :for slot :in (mop:class-slots (find-class type1))
            :for name := (mop:slot-definition-name slot)
            :always (= (funcall (intern (format nil "~s-~s" type1 name)) first)
                       (funcall (intern (format nil "~s-~s" type2 name)) second))))))

(defpolymorph-compiler-macro = (structure-object structure-object) ;; FIXME
    (first second &environment env)
  (let* ((type1        (%form-type first env))
         (type2        (%form-type second env)))
    (unless (eql type1 type2)
      (error "Structures of different types are never ="))
    `(and
      ,@(loop :for slot :in (mop:class-slots (find-class type1 t env))
              :for name := (mop:slot-definition-name slot)
              :for type := (mop:slot-definition-type slot)
              :collect `(= (the ,type (,(intern (format nil "~s-~s" type1 name)) ,first))
                           (the ,type (,(intern (format nil "~s-~s" type2 name)) ,second)))))))

(defpolymorph-compiler-macro = (t t) (first second &environment env)
  (let* ((type1        (%form-type first env))
         (type2        (%form-type second env))
         (intersect   `(and ,type1 ,type2)))
    (if (subtypep intersect nil env)
        (progn
          (warn "Different types equality defaults to nil")
          nil)
        (once-only (first second)
          `(if (and (typep ,first ',intersect)
                  (typep ,second ',intersect))
               (= (the ,intersect ,first) (the ,intersect ,second))
               nil)))))
||#

(defpolymorph (= :inline t) ((first t) (second t) (third t) &rest args) (values boolean &optional)
  "Reduce equality over multiple arguments."
  (cl:reduce (lambda (a b) (and a (= b first)))
             args :initial-value (and (= first second) (= second third))))

(defpolymorph-compiler-macro = (t t t &rest) (&whole form first second third &rest args
                                                     &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (with-type-info (type () env) x type))
                           (cons first (cons second (cons third args))))))
        (if (every (lambda (typename) (subtypep typename 'number env)) types)
            `(cl:= ,first ,second ,third ,@args)
            (labels ((rec (ls res)
                       (if (cdr ls)
                           (rec (cdr ls) (cons `(= ,(car ls) ,(cadr ls))
                                               res))
                           (reverse res))))
              (let ((names (mapcar (lambda(x) (gensym (string x))) args)))
                (once-only (first second third)
                  `(let ,(loop :for name :in names
                               :for arg :in args
                               :collect `(,name ,arg))
                     (declare ,@(loop :for name :in names
                                      :for type :in (cdddr types)
                                      :collect `(type ,type ,name))
                              (type ,(first types) ,first)
                              (type ,(second types) ,second)
                              (type ,(third types) ,third))
                     (not
                      (not
                       (and (= ,first ,second)
                          (= ,second ,third)
                          ,@(rec (cons third names) nil))))))))))
      form))


(defpolymorph (/= :inline t) ((first t)) (eql t)
  "Extend CL:/= inequality to a single argument of any type. Returns T in this case."
  (declare (ignorable first))
  t)

(defpolymorph (/= :inline t) ((first t) (second t)) (values boolean &optional)
  (not (= first second)))

(defpolymorph (/= :inline t) ((first t) (second t) (third t) &rest args) (values boolean &optional)
  (not (cl:reduce (lambda (a b) (and a (= b first)))
                args :initial-value (and (= first second) (= second third)))))

(defpolymorph-compiler-macro /= (t t t &rest) (&whole form first second third &rest args
                                                      &environment env)
  (if (constantp (length args) env)
      `(not (= ,first ,second ,third ,@args))
      form))


;; Order comparison functions
(define-polymorphic-function < (object &rest objects) :overwrite t
  :documentation "Return T if its arguments are in strictly increasing order, NIL otherwise.")

(define-polymorphic-function <= (object &rest objects) :overwrite t
  :documentation "Return T if arguments are in strictly non-decreasing order, NIL otherwise.")

(define-polymorphic-function > (object &rest objects) :overwrite t
  :documentation "Return T if its arguments are in strictly decreasing order, NIL otherwise.
Defined as negation of [POLYMORPH.MATHS:<=][polymorphic-function], therefore works for all the same argument types.
By default those include REALs, CHARACTERs and STRINGs.")

(define-polymorphic-function >= (object &rest objects) :overwrite t
  :documentation "Return T if arguments are in strictly non-increasing order, NIL otherwise.
Defined as negation of [POLYMORPH.MATHS:<][polymorphic-function], therefore works for all the same argument types.
By default those include REALs, CHARACTERs and STRINGs.")


(defpolymorph (> :inline t) ((first t)) (eql t)
  (declare (ignorable first))
  t)

(defpolymorph (>= :inline t) ((first t)) (eql t)
  (declare (ignorable first))
  t)

(defpolymorph (< :inline t) ((first t)) (eql t)
  (declare (ignorable first))
  t)

(defpolymorph (<= :inline t) ((first t)) (eql t)
  (declare (ignorable first))
  t)


(defpolymorph (< :inline t) ((first real) (second real)) (values boolean &optional)
  (cL:< first second))

(defpolymorph (<= :inline t) ((first real) (second real)) (values boolean &optional)
  (cl:<= first second))

(defpolymorph (< :inline t) ((first character) (second character)) (values boolean &optional)
  (char< first second))

(defpolymorph (<= :inline t) ((first character) (second character)) (values boolean &optional)
  (char<= first second))

(defpolymorph (< :inline t) ((first string) (second string)) (values boolean &optional)
  (not (not (string< first second))))

(defpolymorph (<= :inline t) ((first string) (second string)) (values boolean &optional)
  (not (not (string<= first second))))

(defpolymorph (< :inline t) ((first t) (second t) (third t) &rest args)
    (values boolean &optional)
  (not
   (not (and (< first second)
             (< second third)
             (if (not args)
                 t
                 (loop :for (a b) :on (cons third args)
                       :while b
                       :always (< a b)))))))


(defpolymorph-compiler-macro < (t t t &rest) (&whole form first second third &rest args
                                                     &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (with-type-info (type () env) x type))
                           (cons first (cons second (cons third args))))))
        (if (every (lambda (typename) (subtypep typename 'number env)) types)
            `(cl:< ,first ,second ,third ,@args)
            (labels ((rec (ls res)
                       (if (cdr ls)
                           (rec (cdr ls) (cons `(< ,(car ls) ,(cadr ls))
                                               res))
                           (reverse res))))
              (let ((names (mapcar (lambda(x) (gensym (string x))) args)))
                (once-only (first second third)
                  `(let ,(loop :for name :in names
                               :for arg :in args
                               :collect `(,name ,arg))
                     (declare ,@(loop :for name :in names
                                      :for type :in (cdddr types)
                                      :collect `(type ,type ,name))
                              (type ,(first types) ,first)
                              (type ,(second types) ,second)
                              (type ,(third types) ,third))
                     (not
                      (not
                       (and (< ,first ,second)
                            (< ,second ,third)
                            ,@(rec (cons third names) nil))))))))))
      form))


(defpolymorph (<= :inline t) ((first t) (second t) (third t) &rest args)
    (values boolean &optional)
  (not
   (not (and (<= first second)
             (<= second third)
             (if (not args)
                 t
                 (loop :for (a b) :on (cons third args)
                       :while b
                       :always (<= a b)))))))

(defpolymorph-compiler-macro <= (t t t &rest) (&whole form first second third &rest args
                                                      &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (with-type-info (type () env) x type))
                           (cons first (cons second (cons third args))))))
        (if (every (lambda (typename) (subtypep typename 'number env)) types)
            `(cl:<= ,first ,second ,third ,@args)
            (labels ((rec (ls res)
                       (if (cdr ls)
                           (rec (cdr ls) (cons `(<= ,(car ls) ,(cadr ls))
                                               res))
                           (reverse res))))
              (let ((names (mapcar (lambda(x) (gensym (string x))) args)))
                (once-only (first second third)
                  `(let ,(loop :for name :in names
                               :for arg :in args
                               :collect `(,name ,arg))
                     (declare ,@(loop :for name :in names
                                      :for type :in (cdddr types)
                                      :collect `(type ,type ,name))
                              (type ,(first types) ,first)
                              (type ,(second types) ,second)
                              (type ,(third types) ,third))
                     (not
                      (not
                       (and (<= ,first ,second)
                            (<= ,second ,third)
                            ,@(rec (cons third names) nil))))))))))
      form))


(defpolymorph (> :inline t) ((first t) (second t)) (values boolean &optional)
  (not (<= first second)))

(defpolymorph (>= :inline t) ((first t) (second t)) (values boolean &optional)
  (not (< first second)))

(defpolymorph (> :inline t) ((first t) (second t) (third t) &rest args)
    (values boolean &optional)
  (not
   (not (and (> first second)
             (> second third)
             (if (not args)
                 t
                 (loop :for (a b) :on (cons third args)
                       :while b
                       :always (> a b)))))))


(defpolymorph-compiler-macro > (t t t &rest) (&whole form first second third &rest args
                                                     &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (with-type-info (type () env) x type))
                           (cons first (cons second (cons third args))))))
        (if (every (lambda (typename) (subtypep typename 'number env)) types)
            `(cl:> ,first ,second ,third ,@args)
            (labels ((rec (ls res)
                       (if (cdr ls)
                           (rec (cdr ls) (cons `(> ,(car ls) ,(cadr ls))
                                               res))
                           (reverse res))))
              (let ((names (mapcar (lambda(x) (gensym (string x))) args)))
                (once-only (first second third)
                  `(let ,(loop :for name :in names
                               :for arg :in args
                               :collect `(,name ,arg))
                     (declare ,@(loop :for name :in names
                                      :for type :in (cdddr types)
                                      :collect `(type ,type ,name))
                              (type ,(first types) ,first)
                              (type ,(second types) ,second)
                              (type ,(third types) ,third))
                     (not
                      (not
                       (and (> ,first ,second)
                            (> ,second ,third)
                            ,@(rec (cons third names) nil))))))))))
      form))


(defpolymorph (>= :inline t) ((first t) (second t) (third t) &rest args)
    (values boolean &optional)
  (not
   (not (and (>= first second)
             (>= second third)
             (if (not args)
                 t
                 (loop :for (a b) :on (cons third args)
                       :while b
                       :always (>= a b)))))))


(defpolymorph-compiler-macro >= (t t t &rest) (&whole form first second third &rest args
                                                      &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (with-type-info (type () env) x type))
                           (cons first (cons second (cons third args))))))
        (if (every (lambda (typename) (subtypep typename 'number env)) types)
            `(cl:>= ,first ,second ,third ,@args)
            (labels ((rec (ls res)
                       (if (cdr ls)
                           (rec (cdr ls) (cons `(>= ,(car ls) ,(cadr ls))
                                               res))
                           (reverse res))))
              (let ((names (mapcar (lambda(x) (gensym (string x))) args)))
                (once-only (first second third)
                  `(let ,(loop :for name :in names
                               :for arg :in args
                               :collect `(,name ,arg))
                     (declare ,@(loop :for name :in names
                                      :for type :in (cdddr types)
                                      :collect `(type ,type ,name))
                              (type ,(first types) ,first)
                              (type ,(second types) ,second)
                              (type ,(third types) ,third))
                     (not
                      (not
                       (and (>= ,first ,second)
                            (>= ,second ,third)
                            ,@(rec (cons third names) nil))))))))))
      form))


(define-polymorphic-function max (&rest xs) :overwrite t
  :documentation "Returns maximum of provided arguments.
Comparison falls back to the [POLYMORPH.MATHS:<][polymorphic-function]
polymorphic order function.")

(define-polymorphic-function min (&rest xs) :overwrite t
  :documentation "Returns minimum of provided arguments.
Comparison falls back to the [POLYMORPH.MATHS:<][polymorphic-function]
polymorphic order function.")

(defpolymorph max ((a t)) t
  "Obviously returns its single argument when only one is provided."
  a)

(defpolymorph min ((a t)) t
  "Obviously returns its single argument when only one is provided."
  a)

(defpolymorph max ((first t) (second t)) t
  "Returns maximum between FIRST and SECOND argument comparing with
[POLYMORPH.MATHS:<][polymorphic-function]."
  (if (< first second) second first))
  
(defpolymorph min ((first t) (second t)) t
  "Returns minimum between FIRST and SECOND argument comparing with
[POLYMORPH.MATHS:<][polymorphic-function]."
  (if (not (< second first)) first second))


(defpolymorph-compiler-macro max (t t) (first second &environment env)
  (with-type-info (type1 () env) first
    (with-type-info (type2 () env) second
      (let ((name1 (gensym "FIRST"))
            (name2 (gensym "SECOND")))
        `(let ((,name1 ,first)
               (,name2 ,second))
           (declare (type ,type1 ,name1) (type ,type2 ,name2))
           (if (< ,name1 ,name2) ,name2 ,name1))))))


(defpolymorph-compiler-macro min (t t) (first second &environment env)
  (with-type-info (type1 () env) first
    (with-type-info (type2 () env) second
     (let ((name1 (gensym "FIRST"))
           (name2 (gensym "SECOND")))
       `(let ((,name1 ,first)
              (,name2 ,second))
          (declare (type ,type1 ,name1) (type ,type2 ,name2))
          (if (not (< ,name2 ,name1)) ,name1 ,name2))))))


(defpolymorph (max :inline t) ((first t) (second t) (third t) &rest xs) t
  "Reduce MAX over multiple arguments."
  (cl:reduce #'max xs :initial-value (max first (max second third))))


(defpolymorph-compiler-macro max (t t t &rest) (&whole form first second third &rest xs
                                                       &environment env)
  (labels ((genmax (ls done)
             (if ls
                 (genmax (cdr ls) `(max ,done ,(car ls)))
                 done)))
    (let* ((nonconst (remove-if (lambda (x) (constantp x env))
                                (cons first (cons second (cons third xs)))))
           (const (remove-if (lambda (x) (not (constantp x env)))
                             (cons first (cons second (cons third xs)))))
           (full (append const nonconst))
           (first (first full))
           (second (second full))
           (third (third full))
           (xs (cdddr full)))
      (if (constantp (length xs) env)
          (genmax xs `(max ,first (max ,second ,third)))
          form))))


(defpolymorph (min :inline t) ((first t) (second t) (third t) &rest xs) t
  "Reduce MIN over multiple arguments."
  (cl:reduce #'min xs :initial-value (min first (min second third))))


(defpolymorph-compiler-macro min (t t t &rest) (&whole form first second third &rest xs
                                                       &environment env)
  (labels ((genmin (ls done)
             (if ls
                 (genmin (cdr ls) `(min ,done ,(car ls)))
                 done)))
    (let* ((nonconst (remove-if (lambda (x) (constantp x env))
                                (cons first (cons second (cons third xs)))))
           (const (remove-if (lambda (x) (not (constantp x env)))
                             (cons first (cons second (cons third xs)))))
           (full (append const nonconst))
           (first (first full))
           (second (second full))
           (third (third full))
           (xs (cdddr full)))
      (if (constantp (length xs) env)
          (genmin xs `(min ,first (min ,second ,third)))
          form))))


;; Arithmetics
(define-polymorphic-function + (&rest xs) :overwrite t
  :documentation "Arithmetic addition.")

(defpolymorph + () (eql 0)
  "When called without arguments, '+ returns 0."
  0)

(defpolymorph + ((a t)) t
  "When called with only one argument A of any type, return A."
  a)

(defpolymorph + ((first number) (second number)) (values number &optional)
  "Two arguments of type NUMBER are added with CL:+ ."
  (cl:+ first second))

(defpolymorph + ((first character) (second character)) (values character &optional)
  "Addition of CHARACTER type arguments results in CHARACTER with the code as the sum if the arguments' codes."
  (code-char (cl:+ (char-code first) (char-code second))))

(defpolymorph (+ :inline t) ((first t) (second t) (third t) &rest xs) t
  "Reduce summation over multiple arguments."
  (cl:reduce #'+ xs :initial-value (+ (+ first second) third)))


(defpolymorph-compiler-macro + (t t t &rest) (&whole form first second third &rest xs
                                                     &environment env)
  (labels ((gen+ (ls done)
             (if ls
                 (gen+ (cdr ls) `(+ ,done ,(car ls)))
                 done)))
   (if (constantp (length xs) env)
       (gen+ xs `(+ (+ ,first ,second) ,third))
       form)))


(define-polymorphic-function - (x &rest xs) :overwrite t
  :documentation "Arithmetic subtraction.")

(defpolymorph - ((a number)) (values number &optional)
  "Unary negation for NUMBER A, same as CL:- ."
  (cl:- a))

(defpolymorph - ((first number) (second number)) (values number &optional)
  "Subtract NUMBER SECOND from NUMBER FIRST."
  (cl:- first second))

(defpolymorph - ((first character) (second character)) (values character &optional)
  "Subtraction of CHARACTER type arguments results in CHARACTER with the
code as the subtraction of the CHARACTER SECOND from CHARACTER FIRST."
  (code-char (cl:- (char-code first) (char-code second))))

(defpolymorph (- :inline t) ((first t) (second t) (third t) &rest xs) t
  "Reduce subtraction over multiple arguments."
  (cl:reduce #'- xs :initial-value (- (- first second) third)))


(defpolymorph-compiler-macro - (t t t &rest) (&whole form first second third &rest xs
                                                     &environment env)
  (labels ((gen- (ls done)
             (if ls
                 (gen- (cdr ls) `(- ,done ,(car ls)))
                 done)))
   (if (constantp (length xs) env)
       (gen- xs `(- (- ,first ,second) ,third))
       form)))


(define-polymorphic-function * (&rest xs) :overwrite t
  :documentation "Arithmetic multiplication.")

(defpolymorph * () (eql 1)
  "When called without arguments, `'*` returns neutral element 1."
  1)

(defpolymorph * ((a t)) t
  "When called with only one argument A of any type, return A."
  a)

(defpolymorph * ((first number) (second number)) (values number &optional)
  "Multiply NUMBER FIRST and NUMBER SECOND."
  (cl:* first second))

(defpolymorph (* :inline t) ((first t) (second t) (third t) &rest xs) t
  "Reduce multiplication over multiple arguments."
  (cl:reduce #'* xs :initial-value (* (* first second) third)))


(defpolymorph-compiler-macro * (t t t &rest) (&whole form first second third &rest xs
                                                     &environment env)
  (labels ((gen* (ls done)
             (if ls
                 (gen* (cdr ls) `(* ,done ,(car ls)))
                 done)))
   (if (constantp (length xs) env)
       (gen* xs `(* (* ,first ,second) ,third))
       form)))


(define-polymorphic-function / (x &rest xs) :overwrite t
  :documentation "Arithmetic division.")

(defpolymorph / ((a number)) (values number &optional)
  "Inverse of NUMBER A, same as CL:/ ."
  (cl:/ a))

(defpolymorph / ((first number) (second number))
  (values number &optional)
  "Divide NUMBER FIRST with NUMBER SECOND."
  (cl:/ first second))

(defpolymorph (/ :inline t) ((first t) (second t) (third t) &rest xs) t
  "Reduce division over multiple arguments."
  (cl:reduce #'/ xs :initial-value (/ (/ first second) third)))


(defpolymorph-compiler-macro / (t t t &rest) (&whole form first second third &rest xs
                                                     &environment env)
  (labels ((gen/ (ls done)
             (if ls
                 (gen/ (cdr ls) `(/ ,done ,(car ls)))
                 done)))
   (if (constantp (length xs) env)
       (gen/ xs `(/ (/ ,first ,second) ,third))
       form)))


(defmacro incf (place &optional (val 1) &environment env)
   "Like CL:INCF, write a place with its value increased by DELTA.
DELTA defaults to NUMBER 1 but can be any type for which the
[POLYMORPH.MATHS:+][polymorphic-function] is defined."
  (multiple-value-bind
        (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    (let ((place-type (%form-type place env))
          (val-type (%form-type val env)))
      `(let* (,@(mapcar #'list temps exprs)
              (,(car stores)
                (+ (the ,place-type ,access-expr) ,val)))
         (declare (type (and ,place-type ,val-type) ,(car stores)))
         ,store-expr))))

(defmacro decf (place &optional (val 1) &environment env)
  "Like CL:DECF, write a place with its value decreased by DELTA.
DELTA defaults to NUMBER 1."
  (multiple-value-bind
        (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    (let ((place-type (%form-type place env))
          (val-type (%form-type val env)))
      `(let* (,@(mapcar #'list temps exprs)
              (,(car stores)
                (- (the ,place-type ,access-expr) ,val)))
         (declare (type (and ,place-type ,val-type) ,(car stores)))
         ,store-expr))))

(defmacro multf (place &optional (val 1) &environment env)
   "Write a place with its value multiplied by DELTA, which defaults to a
NUMBER 1."
  (multiple-value-bind
        (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    (let ((place-type (%form-type place env))
          (val-type (%form-type val env)))
      `(let* (,@(mapcar #'list temps exprs)
              (,(car stores)
                (* (the ,place-type ,access-expr) ,val)))
         (declare (type (and ,place-type ,val-type) ,(car stores)))
         ,store-expr))))

(defmacro divf (place &optional (val 1) &environment env)
   "Write a place with its value divided by DELTA, which defaults to a
NUMBER 1."
  (multiple-value-bind
        (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    (let ((place-type (%form-type place env))
          (val-type (%form-type val env)))
      `(let* (,@(mapcar #'list temps exprs)
              (,(car stores)
                (/ (the ,place-type ,access-expr) ,val)))
         (declare (type (and ,place-type ,val-type) ,(car stores)))
         ,store-expr))))


(defmacro case= (expr &body forms)
  (let ((res (gensym "RESULT")))
    `(let ((,res ,expr))
       (cond ,@(loop :for (expected actions) :in forms
                     :collect (if (atom expected)
                                  `((= ,res ,expected)
                                    ,actions)
                                  `((or ,@(loop :for ex :in expected
                                                :collect `(= ,res ,ex)))
                                    ,actions)))))))
