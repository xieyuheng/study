# cicada

programming language implementation practices

## Techniques

> Remember,<br>
> an engineer solve problem,<br>
> an engineer does not want just a religion about how to solve a problem.<br>
> -- Gerry Sussman

### study of parsing

- [partech](partech):<br>
  A parser generator, with various parsing techniques
  - [Earley parser](partech/parsing_techniques/Earley.scala)
    - O(n^3) for general context free grammar

### study of scope

- [eopl/lang_let](eopl/lang_let):<br>
  scope!
- [eopl/lang_proc](eopl/lang_proc):<br>
  lexical scope! (closure)
- [eopl/lang_letrec](eopl/lang_letrec):<br>
  recursive scope! (by a trick in the env lookup function)
- [eopl/lang_nameless](eopl/lang_nameless):<br>
  nameless scope! (a.k.a. de bruijn index)

### study of assignment

- [eopl/lang_explicit_refs](eopl/lang_explicit_refs):<br>
  explicit reference (address)
- [eopl/lang_implicit_refs](eopl/lang_implicit_refs):<br>
  distinguish `denoted value` from `expressed value` for mutable variable. <br>
  this is also called call-by-value parameter-passing.
- [eopl/lang_mutable_pairs](eopl/lang_mutable_pairs):<br>
  remember Dan's paper "Cons should not evaluate its arguments" ? <br>
  this is preparation for it.
- [eopl/lang_call_by_need](eopl/lang_call_by_need):<br>
  remember Dan's paper "Cons should not evaluate its arguments" ? <br>
  this is about it. <br>
  call-by-need is also called lazy-eval.
- [eopl/lang_call_by_reference](eopl/lang_call_by_reference):<br>
  caller can mutate reference passed as arg into it. <br>
  this is often seen in imperative language.

### study of simple type

- [de_bruijn](de_bruijn):<br>
  De Bruijn stype simple type lambda calculus
  - implemented by term rewriting

### study of type inference

- [miniml](miniml):<br>
  "A simple applicative language: Mini-ML"
  - by Dominique Clement, Joelle Despeyroux, Thierry Despeyroux, Gilles Kahn

### study of normalization by evaluation (a.k.a. norm-by-eval (a.k.a. NbE))

- [lambda](lambda):<br>
  The untyped lambda calculus
- [syst](syst):<br>
  Kurt Gödel's system T
  - simply typed lambda calculus with natural number,
- [tartlet](tartlet):<br>
  "Checking Dependent Types with Normalization by Evaluation: A Tutorial"
  - this is the little pie, recursion is also not an option here.
  - by David Thrane Christiansen.
    ([the original tutorial](http://davidchristiansen.dk/tutorials/nbe))
- [minitt](minitt):<br>
  "A simple type-theoretic language: Mini-TT"
  - by Thierry Coquand, Yoshiki Kinoshita, Bengt Nordström, Makoto Takeyama
  - no termination check, thus when viewed as logic it is unsound.

### study of function composition and stack machine

- [adventure/jojo_untyped](adventure/jojo_untyped):<br>
  de bruijn notation implemented by threaded code
- [adventure/jojo_simple](adventure/jojo_simple):<br>
  two levels of computations -- `exe` and `cut`
  - `exe` level computation is just computation
  - `cut` level computation is type checking <br>
    thinking about how we implement type checking, <br>
    type checking is compile time computation with good error report, is it not ?
  - `let` is not allowed in `cut` level computation
- [adventure/jojo_dependent](adventure/jojo_dependent):<br>
  the same as `simple`, but allow `let` in `cut`

### references

- Parsing Techniques -- A Practical Guide, Second Edition
  - by Dick Grune and Ceriel J.H. Jacobs
- Essentials of Programming Languages (a.k.a. EOPL)
  - by Daniel P. Friedman and Mitchell Wand
- Lambda Calculus with Types
  - by Henk Barendregt, Wil Dekkers and Richard Statman

## Usage

- Build: `sbt stage`
  - test: `./dev t`
  - find executables at `./target/universal/stage/bin/`
- Example code at: [example](https://github.com/xieyuheng/cicada/tree/master/example)

## Community

- We enforce C4 as collaboration protocol -- [The C4 RFC](https://rfc.zeromq.org/spec:42/C4)
- [Style Guide](STYLE-GUIDE.md) -- observe the style of existing code and respect it
- [Code of Conduct](CODE-OF-CONDUCT.md)
- [Travis CI](https://travis-ci.org/xieyuheng/cicada)

## License

- [GPLv3](LICENSE)
