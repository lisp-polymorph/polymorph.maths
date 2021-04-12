# polymorph.maths
- Equality
``` common-lisp
(define-polymorphic-function = (first second))
(define-polymorphic-function /= (first second))
```
Equality as checking if two objects are clones of each other. Recursive. Should always return boolean.

```common-lisp
(define-polymorphic-function < (first second))
(define-polymorphic-function <= (first second))
(define-polymorphic-function > (first second))
(define-polymorphic-function >= (first second))
```
Standart mathematical comparison. By default is defined by reals, chars and strings. Should alwasy return boolean.



```common-lisp
(define-polymorphic-function + (&rest))
((define-polymorphic-function - (&rest))
d(define-polymorphic-function * (&rest))
efine-polymorphic-function / (&rest))
```
Standart mathematical operations. Defined for numbers. + and - are also defined for chars. Should always return object from the same domain.

