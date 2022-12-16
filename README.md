# polymorph.maths

## Equality
``` common-lisp
(define-polymorphic-function = (&rest args))
(define-polymorphic-function /= (&rest args))
```
Equality checks if all supplied objects are clones of each other. Recursive. Always returns boolean. If only one object is supplied always returns T. If objects of incomparable types are supplied defaults to nil, and if the incomparability of types is known at compile time signals a warning about it.

## Comparison

```common-lisp
(define-polymorphic-function < (&rest args))
(define-polymorphic-function <= (&rest args))
(define-polymorphic-function > (&rest args))
(define-polymorphic-function >= (&rest args))
```
Standard mathematical comparison. By default is defined as reals, chars, and strings. Always returns boolean. If only one object is supplied always returns T. Supplying objects of incomparable types is an error.

```common-lisp
(define-polymorphic-function min (first &rest args))
(define-polymorphic-function max (first &rest args))
```

## Min/Max Funcitons 

Min and max are based on < operator defined above.

## Arithmetics

```common-lisp
(define-polymorphic-function + (&rest args))
(define-polymorphic-function - (x &rest args))
(define-polymorphic-function * (&rest args))
(define-polymorphic-function / (x &rest args))
```
Standard mathematical operations. Defined for numbers. + and - are also defined for chars. Always returns object from the same domain. For numbers adhere to clhs documentation.

