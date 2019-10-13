# cicada

programming language implementation practices

## Techniques

> Remember,<br>
> an engineer solve problem,<br>
> an engineer does not want just a religion about how to solve a problem.<br>
> -- Gerry Sussman

### parsing techniques

- [partech](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/partech):<br>
  A parser generator, with various parsing techniques
  - [Earley parser](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/partech/parsing_techniques/Earley.scala)
    - O(n^3) for general context free grammar

### interpreting techniques

#### EOPL

study of scope:
- [lang_let](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_let):<br>
  scope!
- [lang_proc](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_proc):<br>
  lexical scope! (closure)
- [lang_letrec](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_letrec):<br>
  recursive scope! (by a trick in the env lookup function)
- [lang_nameless](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_nameless):<br>
  nameless scope! (a.k.a. de bruijn index)

study of assignment:
- [lang_explicit_refs](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_explicit_refs):<br>
  explicit reference (address)
- [lang_implicit_refs](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_implicit_refs):<br>
  distinguish `denoted value` from `expressed value` for mutable variable. <br>
  this is also called call-by-value parameter-passing.
- [lang_mutable_pairs](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_mutable_pairs):<br>
  remember Dan's paper "Cons should not evaluate its arguments" ? <br>
  this is preparation for it.
- [lang_call_by_need](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_call_by_need):<br>
  remember Dan's paper "Cons should not evaluate its arguments" ? <br>
  this is about it. <br>
  call-by-need is also called lazy-eval.
- [lang_call_by_reference](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_call_by_reference):<br>
  caller can mutate reference passed as arg into it. <br>
  this is often seen in imperative language.

### type checking techniques

study of simple type:
- [de_bruijn](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/de_bruijn):<br>
  De Bruijn stype simple type lambda calculus
  - implemented by term rewriting

study of type inference:
- [miniml](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/miniml):<br>
  "A simple applicative language: Mini-ML"
  - by Dominique Clement, Joelle Despeyroux, Thierry Despeyroux, Gilles Kahn

study of normalization by evaluation (a.k.a. norm-by-eval (a.k.a. NbE)):
- [lambda](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/lambda):<br>
  The untyped lambda calculus
- [syst](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/syst):<br>
  Kurt Gödel's system T
  - simply typed lambda calculus with natural number,
- [tartlet](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/tartlet):<br>
  "Checking Dependent Types with Normalization by Evaluation: A Tutorial"
  - this is the little pie, recursion is also not an option here.
  - by David Thrane Christiansen.
    ([the original tutorial](http://davidchristiansen.dk/tutorials/nbe))
- [minitt](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/minitt):<br>
  "A simple type-theoretic language: Mini-TT"
  - by Thierry Coquand, Yoshiki Kinoshita, Bengt Nordström, Makoto Takeyama
  - no termination check, thus when viewed as logic it is unsound.

### bizarre techniques

study of function composition and stack machine:
- [untyped](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/adventure/untyped):<br>
  de bruijn notation implemented by threaded code
- [simple](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/adventure/simple):<br>
  two levels of computations -- `exe` and `cut`
  - `exe` level computation is just computation
  - `cut` level computation is type checking <br>
    thinking about how we implement type checking, <br>
    type checking is compile time computation with good error report, is it not ?
  - `let` is not allowed in `cut` level computation
- [dependent](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/adventure/dependent):<br>
  the same as `simple`, but allow `let` in `cut`

### references

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
