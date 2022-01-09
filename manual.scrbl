#lang scribble/manual

@(require
  racket
  racket/function
  scribble/core
  "interpreter.rkt"
  "scribble-utensils.rkt"
  racket/block
  (for-label
   "interpreter.rkt"
   (except-in racket set)
   racket/block
   racket/function)
  (for-template
   "interpreter.rkt"
   (except-in racket set)
   racket/function
   racket/block)
  (for-syntax
   (except-in racket set)
   racket/function
   racket/block))

@title[#:version ""]{Meta-recursive interpreter@(lb)inspired by The Little LISPer}
@author{Jacob J. A. Koot}

@(defmodule The-Little-LISPer/interpreter #:packages ())
@;@(defmodule "interpreter.rkt" #:packages ())

@section{Introduction}
The penultimate question and answer in
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}
by Danial P. Friedman and Matthias Felleisen
@nb{(1989, ISBN 0-574-24005-5)} read:

@inset{@Tabular[
(((list @nb{Does that mean we can run the interpreter}@(lb)
        @nb{on the interpreter if we do the transforma-}@(lb)
        @nb{tion with the Y-combinator.})
  @nb{Yes, but don't bother.}))
 #:sep (hspace 5)
 #:row-properties '((top top-border bottom-border))]}

Well, I do bother and therefore I'm giving it a shot in the form of two modules:

@inset{@nbhl["interpreter.rkt"]{interpreter.rkt}@(lb)
@nbhl["restrictions.rkt"]{restrictions.rkt}}

Submodule @nbhl["restrictions.rkt"]{@nbr[(submod "restrictions.rkt" restrictions)]}
defines a restricted language for the
@nbr[source-code] in module @nbhl["interpreter.rkt"]{interpreter.rkt}.
It also provides all primitives and macros that are used in the @nbr[source-code].
Function @nbr[value] as provided by @nbhl["interpreter.rkt"]{interpreter.rkt}
is not a straight forward transformation of function @tt{value} of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The little LISPer}.
Functions and macros are represented by procedures of three arguments:

@inset{@elemtag{function/macro}
         
@defproc[#:kind "function or macro" #:link-target? #f
(function/macro
 (exprs list?)
 (env (-> symbol? any/c))
 (eval #,(nber "function/macro" "function/macro?")))
         any/c]{
The @nbr[exprs] are the unevaluated arguments.
@nbr[env] is the environment and @nbr[eval] a function for evaluation of the @nbr[exprs].
A function always uses @nbr[env] and @nbr[eval]
for the evaluation of the @nbr[exprs] and thereafter uses the values only.
A macro has more freedom.
Function @nbr[eval] reveives itself as argument when it is called by
procedure @nbr[value].
@nbhl["restrictions.rkt"]{@nbr[(submod "restrictions.rkt" restrictions)]}
provides primitive functions for the @nbr[source-code].
Within the latter they are wrapped such as to become @(nber "function/macro" "functions")
in the required representation.}}

The language implemented by function @nbr[value] is less restricted than that defined by
@nbr[(submod "restrictions.rkt" restrictions)]. In fact the @nbr[source-code] is a
@nbr[let*]-form. This enhances readability for the human eye.
In @nbhl["restrictions.rkt"]{restrictions.rkt}, @nbr[let*] is redefined such as to expand to
a nested @nbr[lambda]-form. Function @nbr[value] implements @nbr[let*] in the same way.

@section[#:tag "restrictions"]{Restrictions on the source-code}

The restrictions imposed on the @nbr[source-code] include the five laws at the inside of the back
cover of @nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}
and allow a restricted set of primitives.
The following restrictions apply to the @nbr[source-code].
@itemlist[
@item{The macros are @nbr[lambda], @nbr[quote], @nbr[cond] and @nbr[let*].@(lb)
They are restricted as described below.}
@item{The functions are @nbr[atom?], @nbr[symbol?], @nbr[eq?], @nbr[null?], @nbr[cons], @nbr[car],
@nbr[cdr] and @nbr[show].@(lb) Described below too.}]

@defform-remove-empty-lines[@defmacro[(lambda (formal-arg ...+) body)
#:grammar ((formal-arg symbol) (body sexpr))]{
At least one @nbr[formal-arg] is required and
the @nbr[body] is restricted to one @nbr[sexpr] only.
@nb{No optional} nor keyword-arguments.}]

@defform[(quote datum) #:grammar ((datum sexpr))]{As in @(Rckt) but the @nbr[datum]
must be a @nbr[sexpr].}

@defform[(cond (test sexpr) ...+)]{
At least one @nbr[(test sexpr)] clause is required. Each @nbr[test] must yield a
@nbrl[boolean?]{boolean}. This is tested at run-time up to and including the first @nbr[test]
that yields @nbr[#t] (or does not yield a @nbrl[boolean?]{boolean}).}

@defform[(let* ((var sexpr) ...+) body)]{
The @nbr[body] is restricted to one @nbr[sexpr] only.
@nbr[let*]-forms are not allowed in the @nbr[sexpr]s nor in the @nbr[body].
In fact the @nbr[source-code] has the form @nbr[(let* ((var sexpr) ...+) value)]
without any nested @nbr[let*]-form.}

@@defproc[#:kind "predicate" (atom? (obj any/c)) boolean?]{
Same as @nbr[(not (pair? x))]. The interpreter cannot use @nbr[*atom?],
because it accepts everything else than a symbol or a non empty list as self-evaluating atoms too.}

@defproc[#:kind "predicate" (symbol? (obj any/c)) boolean?]{
Same as in @(Rckt).}

@defproc[(eq? (x atom?) (y atom?)) boolean?]{
Equivalence relation @nbr[eq?] is restricted to atoms as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@defproc[(null? (lst list?)) boolean]{
Restricted to lists as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@defproc[(cons (kar any/c) (kdr list?)) list?]{
Argument @nbr[kdr] must be a list as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@defproc[(car (lst (non-empty-listof any/c))) any/c]{
Restricted to proper lists as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@defproc[(cdr (lst (non-empty-listof any/c))) list?]{
Restricted to proper lists as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@defproc[(show (obj any/c)) any/c]{
Returns the @nbr[obj] with the side effect of printing it.}

@section{The source-code}
The code of procedure @nbr[value] is in a submodule of the same name in file
@nbhl["interpreter.rkt"]{interpreter.rkt}.
which is restricted to what is provided by @nbr[(submod "restrictions.rkt" restrictions)], id est,
added, restricted or modified variants of @nbr[lambda], @nbr[quote], @nbr[cond], @nbr[let*],
@nbr[atom?], @nbr[symbol?], @nbr[eq?], @nbr[null?], @nbr[cons], @nbr[car], @nbr[cdr],
and @nbr[show]. These are described in section @seclink["restrictions"]{Restrictions}.
Submodule @tt{value} exports procedure @nbr[value] and its @nbr[source-code].

@defproc[(value (expr any/c)) any/c]{
Called from @[Rckt] procedure @nbr[value] receives the evaluated argument @nbr[expr].
Procedure @nbr[value] evaluates the received value according to its own rules.}

@defthing[source-code sexpr?]{
Source code of procedure @nbr[value] written according to the rules described in section
 @seclink["restrictions"]{Restrictions}.}

@defproc[#:kind "predicate" (*atom? (obj any/c)) boolean?]{
Same as @nbr[(or (null? obj) (symbol? obj) (boolean? obj))].
Used to check the restrictions on the @nbr[source-code],
which itself uses the less restricted predicate @nbr[atom?]
because it must recognize procedures as atoms too.}

@defproc[(sexpr? (obj any/c)) boolean?]{
Same as @nbr[(or (*atom? obj) (and (list? obj) (andmap sexpr? obj)))].}

@section{Language accepted by interpreter.}

Function @nbr[value] evaluates its @tt{@italic{expr}} as follows:

@itemize[
@item{A symbol is looked up in the current environment.
The top-level environment contains: 

@inset{@nbr[atom?], @nbr[symbol?], @nbr[boolean?], @nbr[zero?], @nbr[add1], @nbr[sub1], @nbr[eq?],
@nbr[null?], @nbr[cons], @nbr[list], @nbr[length], @nbr[car], @nbr[cdr], @nbr[number?]
@nbr[+], @nbr[-] @nbr[*], @nbr[=], @nbr[<], @nbr[quotient], @nbr[lambda], @nbr[let*], @nbr[quote],
@nbr[cond] and @nbr[show]}}

@item{A list of empty lists represents a natural number and is self-evaluating.
The numerical functions mentioned in the previous item work with this representation.
Function @nbr[-] returns @nbr[()], id est, zero, if otherwise the result would be negative.}
 
@item{A non-empty proper list (which is not a number)
is evaluated by evaluating the first element,
which is assumed to produce a macro or a function.
Subsequently the macro or function is called.
A functions takes care of the evaluation of its arguments.}

@item{Everything else than a symbol or a proper list is self-evaluating.}]

Examples:

@Interaction[
(value '(add1 (()()())))
(value '((lambda (x) (x (x ()))) add1))]

We'd better allow costumary natural numbers. Therefore we define:

@Interaction*[
(define (trafo expr)
 (cond
  ((natural? expr) (make-list expr '()))
  ((list? expr) (map trafo expr))
  (else expr)))
(code:line)
(define (retro expr)
 (define (natural? expr) (and (list? expr) (andmap null? expr)))
 (cond
  ((natural? expr) (length expr))
  ((list? expr) (map retro expr))
  (else expr)))
(code:line)
(define (*value expr) (retro (value (trafo expr))))
(code:line)
(*value '(list (+ 3 4) (- 5 3) (- 3 5) (* 3 4) (quotient 10 3)))]

Function @nbr[value] itself remains working with lists of empty lists.
A more elaborated example:

@Interaction*[
(*value
'(let*
  ((self-apply (lambda (f) (f f)))
   (Y3
    (lambda (g)
     (self-apply
      (lambda (f)
       (g (lambda (x y z) ((f f) x y z)))))))
   (fibonacci
    (Y3
     (lambda (fibonacci)
      (lambda (first second n)
       (cond
        ((zero? n) (cons first (cons second '())))
        (#t (cons first (fibonacci second (+ first second) (sub1 n))))))))))
  (fibonacci 0 1 10)))]

@section{Meta-recursivity}

Meta-recursivity means that the procedure must be able to evaluate its own source code.
It must even be able to do so at depth of meta-recursion.
(A check of one meta-level only does not prove true meta-recursivity.
Even a check of more than one meta-level deep is no proof.)

@Interaction*[
(define |(value source-code)| (value source-code))
|(value source-code)|]

However, the representation of the function returned by @nbr[value] is such that
it cannot be called from @(Rckt).

@Interaction*[
(|(value source-code)| 'whatever)]

Yet the function can be used, but only from within the interpreter:

@Interaction*[
(value `(,|(value source-code)| '(list 'a 'b 'c)))]

We can even do a muliple level of meta-recursion:

@Interaction*[
(time (code:comment "Two levels deep.")
 (*value
 `(,source-code
  '(,source-code
   '((lambda (x y) (x (y 5 4))) add1 *)))))]

Well, that is not fast, as we could have expected beforehand.@(lb)
To conclude a bogus example:

@Interaction*[
((value '(lambda (x) x))
'(monkey)               (code:comment "List of unevaluated arguments.")
 (lambda (x) x)         (code:comment "A phony environment.")
 (lambda (x y z) (y x)) (code:comment "A phony evaluator."))]

@bold{@larger{The end}}
