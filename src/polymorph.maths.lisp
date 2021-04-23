;;;; polymorph.maths.lisp

(in-package #:polymorph.maths)


(defun %form-type (form &optional env)
  (if (constantp form env)
      (let ((val (eval form))) ;;need custom eval that defaults to sb-ext:eval-in-lexenv here)
        (if (typep val '(or number character symbol))
            (values `(eql ,val) t)
            (values (type-of val) t)))
      (adhoc-polymorphic-functions::form-type form env)))

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
;;TODO This one ^ is BAD, so i gotta rewrite it. Don't use anywhere for now



;; Equality
(define-polymorphic-function = (object &rest objects) :overwrite t
  :documentation "Return T if all of its arguments are , NIL otherwise.")

(define-polymorphic-function /= (object &rest objects) :overwrite t
  :documentation "Return T if all of its arguments are , NIL otherwise.")

(defpolymorph (= :inline t) ((first t)) (eql t)
  (declare (ignorable first))
  t)


(defpolymorph (/= :inline t) ((first t)) (eql t)
  (declare (ignorable first))
  t)

(defpolymorph (= :inline t) ((first number) (second number)) boolean
  (cl:= first second))


(defpolymorph (= :inline t) ((first symbol) (second symbol)) boolean
  (eql first second))

(defpolymorph (= :inline t) ((first character) (second character)) boolean
  (char= first second))

(defpolymorph (= :inline t) ((first string) (second string)) boolean
  (string= first second))


(defpolymorph (= :inline t) ((first cons) (second cons)) boolean
              (and (= (car first) (car second))
                 (= (cdr first) (cdr second))))



(defpolymorph (= :inline t) ((first array)
                             (second array))
    boolean

  (let ((s1 (array-total-size first))
        (s2 (array-total-size second)))
    (and (cl:= s1 s2)
       (equal (array-dimensions first) (array-dimensions second))
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
         (size1 (cond
                    ((and (listp dim1) (every (lambda (x) (constantp x env)) dim1))
                     (reduce (lambda (a b) `(cl:* ,a ,b)) dim1))
                    (t `(array-total-size ,first))))
         (size2 (cond
                    ((and (listp dim2) (every (lambda (x) (constantp x env)) dim2))
                     (reduce (lambda (a b) `(cl:* ,a ,b)) dim2))
                    (t `(array-total-size ,second))))
         (s1 (gensym))
         (s2 (gensym))
         (i (gensym)))
    (print env)
    ;(%check-container-elem-applicable elt1 elt2 #'=)
    (unless (equalp dim1 dim2)
      (warn "Arrays dimensions are not known to be compatbile"))
    (once-only (first second)
      `(let ((,s1 ,size1)
             (,s2 ,size2))
         (and (cl:= ,s1 ,s2)
            (equal (array-dimensions ,first) (array-dimensions ,second))
            (loop :for ,i :below ,s1
                  :always (= (the ,elt1 (row-major-aref ,first ,i))
                             (the ,elt2 (row-major-aref ,second ,i)))))))))


(defpolymorph (= :inline t) ((first (and vector (not simple-array)))
                             (second (and vector (not simple-array))))
    boolean
  (let ((s1 (length first))
        (s2 (length second)))
    (and (cl:= s1 s2)
       (cl:= (fill-pointer first) (fill-pointer second))
       (loop :for i :below s1
             :always (= (aref first i)
                        (aref second i))))))


(defpolymorph-compiler-macro = ((and vector (not simple-array))
                                (and vector (not simple-array)))
    (first second &environment env)
  (let* ((type1 (%form-type first env))
         (elt1  (cm:array-type-element-type type1))
         (type2 (%form-type second env))
         (elt2  (cm:array-type-element-type type2))
         (s1 (gensym))
         (s2 (gensym))
         (i (gensym)))
    ;(%check-container-elem-applicable elt1 elt2 #'=)
    (once-only (first second)
      `(let ((,s1 (length ,first))
             (,s2 (length ,second)))
         (and (cl:= ,s1 ,s2)
            (cl:= (fill-pointer ,first) (fill-pointer ,second))
            (loop :for ,i :below ,s1
                  :always (= (the ,elt1 (aref ,first ,i))
                             (the ,elt2 (aref ,second ,i)))))))))



(defpolymorph = ((first hash-table) (second hash-table)) boolean
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
    boolean
  (let* ((type1        (type-of first))
         (type2        (type-of second)))
   (and (eql type1 type2)
      (loop :for slot :in (mop:class-slots (find-class type1))
            :for name := (mop:slot-definition-name slot)
            :always (= (slot-value first name) (slot-value second name))))))

(defpolymorph-compiler-macro = (structure-object structure-object)
    (first second &environment env)
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


(defpolymorph (= :inline t) ((first t) (second t)) boolean
    (declare (ignorable first second))
    nil)

(defpolymorph-compiler-macro = (t t) (first second)
    (declare (ignorable first second))
    (warn "Different types equality defaults to nil")
    nil)


(defpolymorph (= :inline t) ((first t) (second t) (third t) &rest args) boolean
  (cl:reduce (lambda (a b) (and a (= b first)))
             args :initial-value (and (= first second) (= second third))))



(defpolymorph-compiler-macro = (t t t &rest) (&whole form first second third &rest args
                                                     &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (%form-type x env))
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
  (declare (ignorable first))
  t)

(defpolymorph (/= :inline t) ((first t) (second t)) boolean
  (not (= first second)))

(defpolymorph (/= :inline t) ((first t) (second t) (third t) &rest args) boolean
  (not (cl:reduce (lambda (a b) (and a (= b first)))
                args :initial-value (and (= first second) (= second third)))))

(defpolymorph-compiler-macro /= (t t t &rest) (&whole form first second third &rest args
                                                      &environment env)
  (if (constantp (length args) env)
      `(not (= ,first ,second ,third ,@args))
      form))


;; Inequality
(define-polymorphic-function < (object &rest objects) :overwrite t
  :documentation "Return T if its arguments are in strictly increasing order, NIL otherwise.")
(define-polymorphic-function <= (object &rest objects) :overwrite t
  :documentation "Return T if arguments are in strictly non-decreasing order, NIL otherwise.")
(define-polymorphic-function > (object &rest objects) :overwrite t
  :documentation "Return T if its arguments are in strictly increasing order, NIL otherwise.")
(define-polymorphic-function >= (object &rest objects) :overwrite t
  :documentation "Return T if arguments are in strictly non-decreasing order, NIL otherwise.")

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



(defpolymorph < ((first real) (second real)) boolean
              (cL:< first second))

(defpolymorph <= ((first real) (second real)) boolean
              (cl:<= first second))

(defpolymorph < ((first character) (second character)) boolean
              (char< first second))

(defpolymorph <= ((first character) (second character)) boolean
              (char<= first second))

(defpolymorph < ((first string) (second string)) boolean
              (not (not (string< first second))))

(defpolymorph <= ((first string) (second string)) boolean
              (not (not (string<= first second))))

(defpolymorph (< :inline t) ((first t) (second t) (third t) &rest args)
    boolean
  (flet ((%%< (a b)
           (when (< a b) b)))
    (not (not (and (< first second) (< second third) (cl:reduce #'%%< (cons third args)))))))



(defpolymorph-compiler-macro < (t t t &rest) (&whole form first second third &rest args
                                                     &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (%form-type x env))
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
    boolean
  (flet ((%%<= (a b)
           (when (<= a b) b)))
    (not (not (and (<= first second) (<= second third) (cl:reduce #'%%<= (cons third args)))))))

(defpolymorph-compiler-macro <= (t t t &rest) (&whole form first second third &rest args
                                                     &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (%form-type x env))
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


(defpolymorph (> :inline t) ((first t) (second t)) boolean
  (not (<= first second)))
(defpolymorph (>= :inline t) ((first t) (second t)) boolean
  (not (< first second)))

(defpolymorph (> :inline t) ((first t) (second t) (third t) &rest args)
    boolean
  (flet ((%%> (a b)
           (when (> a b) b)))
    (not (not (and (> first second) (> second third) (cl:reduce #'%%> (cons third args)))))))

(defpolymorph-compiler-macro > (t t t &rest) (&whole form first second third &rest args
                                                     &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (%form-type x env))
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
    boolean
  (flet ((%%>= (a b)
           (when (>= a b) b)))
    (not (not (and (>= first second) (>= second third) (cl:reduce #'%%>= (cons third args)))))))


(defpolymorph-compiler-macro >= (t t t &rest) (&whole form first second third &rest args
                                                     &environment env)

  (if (constantp (length args) env)
      (let ((types (mapcar (lambda (x) (%form-type x env))
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




(define-polymorphic-function max (&rest xs) :overwrite t)
(define-polymorphic-function min (&rest xs) :overwrite t)

(defpolymorph max ((a t)) t
  a)
(defpolymorph min ((a t)) t
  a)

(defpolymorph max ((first number) (second number)) number
  (if (> first second) first second))
(defpolymorph min ((first number) (second number)) number
  (if (<= first second) first second))

(defpolymorph (max :inline t) ((first t) (second t) (third t) &rest xs) t
  (cl:reduce #'max xs :initial-value (max (max first second) third)))

(defpolymorph-compiler-macro max (t t t &rest) (&whole form first second third &rest xs
                                                       &environment env)
  (labels ((genmax (ls done)
             (if ls
                 (genmax (cdr ls) `(max ,done ,(car ls)))
                 done)))
    (if (constantp (length xs) env)
        (genmax xs `(max (max ,first ,second) ,third))
        form)))

(defpolymorph (min :inline t) ((first t) (second t) (third t) &rest xs) t
  (cl:reduce #'min xs :initial-value (min (min first second) third)))

(defpolymorph-compiler-macro min (t t t &rest) (&whole form first second third &rest xs
                                                       &environment env)
  (labels ((genmin (ls done)
             (if ls
                 (genmin (cdr ls) `(min ,done ,(car ls)))
                 done)))
    (if (constantp (length xs) env)
        (genmin xs `(min (min ,first ,second) ,third))
        form)))



;; Arithmetics
(define-polymorphic-function + (&rest xs) :overwrite t)

(defpolymorph + () (eql 0)
  0)


(defpolymorph + ((a t)) t
  a)

(defpolymorph + ((first number) (second number)) number
  (cl:+ first second))

(defpolymorph + ((first character) (second character)) character
  (code-char (cl:+ (char-code first) (char-code second))))

(defpolymorph (+ :inline t) ((first t) (second t) (third t) &rest xs) t
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

(define-polymorphic-function - (x &rest xs) :overwrite t)

(defpolymorph - ((a number)) number
  (cl:- a))

(defpolymorph - ((first number) (second number)) number
  (cl:- first second))

(defpolymorph - ((first character) (second character)) character
  (code-char (cl:- (char-code first) (char-code second))))



(defpolymorph (- :inline t) ((first t) (second t) (third t) &rest xs) t
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

(define-polymorphic-function * (&rest xs) :overwrite t)

(defpolymorph * () (eql 1)
  1)

(defpolymorph * ((a t)) t
  a)

(defpolymorph * ((first number) (second number)) number
  (cl:* first second))

(defpolymorph (* :inline t) ((first t) (second t) (third t) &rest xs) t
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

(define-polymorphic-function / (x &rest xs) :overwrite t)

(defpolymorph / ((a number)) number
  (cl:/ a))

(defpolymorph / ((first number) (second number)) number
  (cl:/ first second))



(defpolymorph (/ :inline t) ((first t) (second t) (third t) &rest xs) t
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




#||
(deftype has-binops (&rest functions)
  (let ((intersec))
    (loop :for fn :in functions
          :for lists := (mapcar #'adhoc-polymorphic-functions::polymorph-type-list
                                (adhoc-polymorphic-functions::polymorphic-function-polymorphs
                                 (fdefinition fn)))
          :for res := (loop :for list :in lists
                            :when (and (= 2 (length list))
                                     (eql (first list) (second list)))
                            :collect (first list))
          :do (setf intersec (if intersec (intersection res intersec) res)))
    `(or ,@intersec)))
||#
