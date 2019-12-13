# party
- clean up partech -- for cicada
- good error report for earley
  like: https://github.com/kach/nearley/issues/451
# cicada
- built-in equality `constrain` to define `eqv_t` as class
- [note] no auto currying
- [less important] Exp use cofree
- [maybe] nymph
  - try a version of nbe with Tl of one field, learn from nbe
# record type
- telescope of automath
- one of the most important techniques we need to learn
  is to handle scope of record-like structure
  such as module, object and class.
- [eopl.lang_module]
  what is the different between first class module and record type?
- [eopl.lang_class]
- [eopl] note about further reading
# partech
- [partech] ErrMsg and Span
- [partech] better report with ErrMsg in context
# english grammar
- to design good programming language,
  we need to study english grammar
# inductive type
- we need two implement languages (one for cicada, one for jojo),
  with arbitrary inductive type definition
  and arbitrary recursive function.
  because we need such languages to do experiments.
- we do not care about consistency for now
- we also allow type in type
# jojo
- the cut level is stable
  only [x : A] and (- A)
  exe level jo in eval is also simple
  but exe level jo in cut can generate different pattern of cut level jo
- how to handle inductive type in jojo ?
- [adventure] both `(let x)` and `[x]`
- [adventure] syntax for explicit tail call
- [adventure.simple] `type <name> = { ... }`
- [adventure.simple] representing data types
- main adventure
  the algebraic structure is simple
  the hardest part is to implement `eqv` between `jojo_t`
  - [adventure.counted] annotation for arg_count and ret_count
  - [adventure.counted] infer arg_count and ret_count
  - [adventure.counted] eq by eval
  - [adventure.counted] encode permutation group
# deduction
- [deduction] specify the syntax
- [deduction] try the deduction syntax on other logic (modal logic)
# sequent
- [sequent] syntax for sequent calculus style deduction
  - we already have natural deduction style
# automath
- jojo is real close to automath
# DIY type theory
- implement proof checker first
  and view type checker as generating proofs
# de_bruijn
- [de_bruijn] `let type <name> = ...`
- [de_bruijn] representing data types
- [de_bruijn_logical]
  - view type atom as logic variable and use unification
  - this should be a different language from [de_bruijn]
- [de_bruijn] synth
- theory about equality between lambda terms
  - reflexivity
    symmetry
    transitivity
    congruence
    beta_reduction
    eta_reduction
# the little mler
- as test for some of the languages
# lambda
- church's encoding and the unsolvable problem paper
- can norm-by-eval handle call-by-name terms ?
- what are examples of call-by-value bad terms ?
# curry
- [curry] view type atom as logic variable and use unification
  - type system of curry style need type variable
    there is no version of curry style without type variable
# tartlet
- [tartlet] extend tartlet to pie
- [tartlet] use `TODO` as un-implemented
- [tartlet] block of let and return
- [tartlet] extend tartlet to define new inductive types
  - keep it subrecursive
  - generate data and ind, rec and iter
- [tartlet] add cumulative (maybe non-cumulative) levels of types -- type^n
- [to be sure] why we *do need* "type directed norm-by-eval" here ?
- [to be sure] about the use of TheVal and friends
- [test] eta conversion: f == (x) => f(x)
# the little typer
- as test for dependently typed language
# pure type system
- in addition to a file to eval, also a file of config of sorts, axioms and rules
- logic framework is a special case of pure type system ?
  plus a single subtyping-like rule
  A: Set --> A: Type
  - where `Set` is inductively generate datatype
# sysf
# minitt
- [note] why we *do not need* "type directed norm-by-eval" here ?
- [question] I think the use of NormEnv will break `equals`
- add `eqv_t` `same` `replace`
- add constrains to type constructors
- why not jsut use letrec for every definition ? -- learn from EOPL
- add cumulative (maybe non-cumulative) levels of types -- type_t(n)
- be sure about the use of Pattern
  - write some notes
  - Norm do not use Pattern
  - note about how letrec is handled at lookup-time
# module system
# compile with cps
# sat
- sat solver visualization (how ?)
# complexity
- cook's machine for trans algo to lin-algo
# lambda machine
- new machine for call-by-name or call-by-need lambda
# term rewriting
# dsl
- generative art
- formal concept analysis
- four dancing circles
# for readme
- [barendregt/curry](src/main/scala/xieyuheng/barendregt/curry) <br>
  - Curry style simple type lambda calculus.
- [pure](src/main/scala/xieyuheng/pure) <br>
  - A framework for the essence of dependent type system, <br>
    which can be viewed as an extension of Barendregt's lambda cube.
- [adventure/jojo_counted](src/main/scala/xieyuheng/adventure/jojo_counted) <br>
  - The language `jojo_untyped` plus annotation for number of arguments and number of return values.
  - An equivalent predicate for jojo, inspired by `nbe`.
- [adventure/jojo_dependent](src/main/scala/xieyuheng/adventure/jojo_dependent) <br>
  - Two level of compositions -- `exe` and `cut`, where `cut` is as powerful as `exe`.
# formal concept analysis
- intention as understanding
  two different group of intentions
  over the same group of extentions
  are two different understanding
# method of analytic tableaux
# formal specification
