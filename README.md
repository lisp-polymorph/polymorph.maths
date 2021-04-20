# polymorph.maths
- Equality
``` common-lisp
(define-polymorphic-function = (&rest args))
(define-polymorphic-function /= (&rest args))
```
Equality as checking if all supplied objects are clones of each other. Recursive. Always return boolean. If only one object is supplied always return T. If objects of incomparable types are supplied default to nil, and if the incomparability of types is known at compile time signal a warning about it.

```common-lisp
(define-polymorphic-function < (&rest args))
(define-polymorphic-function <= (&rest args))
(define-polymorphic-function > (&rest args))
(define-polymorphic-function >= (&rest args))
```
Standart mathematical comparison. By default is defined by reals, chars and strings. Always return boolean. If only one object is supplied always return T. Supplying objects of incomparable types is an error.



```common-lisp
(define-polymorphic-function + (&rest args))
((define-polymorphic-function - (x &rest args))
d(define-polymorphic-function * (&rest args))
efine-polymorphic-function / (x &rest args))
```
Standart mathematical operations. Defined for numbers. + and - are also defined for chars. Should always     return object from the same domain.

