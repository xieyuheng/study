# Cicada

Study of language design and implementation

> Remember, <br>
> an engineer solve problem, <br>
> an engineer does not want just a religion about how to solve a problem. <br>
> -- Gerry Sussman

### Study of parsing techniques

- [partech](src/main/scala/xieyuheng/partech) <br>
  A parser generator, with various parsing techniques.
  - [Earley parser](src/main/scala/xieyuheng/partech/parsing_techniques/Earley.scala) <br>
    Its time complexity is O(n^3) in general for context free grammar.

### Study of scope

- [eopl/lang_let](src/main/scala/xieyuheng/eopl/lang_let) <br>
  - Basic scope.
- [eopl/lang_proc](src/main/scala/xieyuheng/eopl/lang_proc) <br>
  - Lexical scope and closure.
- [eopl/lang_letrec](src/main/scala/xieyuheng/eopl/lang_letrec) <br>
  - Recursive scope, implemented by a trick during variable lookup.
- [eopl/lang_nameless](src/main/scala/xieyuheng/eopl/lang_nameless) <br>
  - Nameless scope, a.k.a. De Bruijn index.

### Study of assignment

- [eopl/lang_explicit_refs](src/main/scala/xieyuheng/eopl/lang_explicit_refs) <br>
  - Explicit reference (address)
- [eopl/lang_implicit_refs](src/main/scala/xieyuheng/eopl/lang_implicit_refs) <br>
  - We need to distinguish `denoted value` from `expressed value` for mutable variable. <br>
    This is also called call-by-value parameter passing.
- [eopl/lang_mutable_pairs](src/main/scala/xieyuheng/eopl/lang_mutable_pairs) <br>
  - Lisp's `cons`, `car` and `cdr`, with `set-car!` and `set-cdr!`
- [eopl/lang_call_by_reference](src/main/scala/xieyuheng/eopl/lang_call_by_reference) <br>
  - Marking an variable `ref` when passing it to a function, <br>
    will not create new `ref` and reuse the `ref` of the variable, <br>
    thus make callee be able to set caller's variable.
- [eopl/lang_call_by_need](src/main/scala/xieyuheng/eopl/lang_call_by_need) <br>
  - When an expression occurs as argument of an application, <br>
    do not eval it, but make it into a `thunk`. <br>
    and only eval it when needed (during variable lookup).
  - This is also called lazy-eval.

### Study of lambda calculus with types

- [barendregt/de_bruijn](src/main/scala/xieyuheng/barendregt/de_bruijn) <br>
  - De Bruijn style simple type lambda calculus, <br>
    implemented by term rewriting.
- **TODO** [barendregt/curry](src/main/scala/xieyuheng/barendregt/curry) <br>
  - Curry style simple type lambda calculus.

### Study of pure type system

- **TODO** [pure](src/main/scala/xieyuheng/pure) <br>
  - A framework for the essence of dependent type system, <br>
    which can be viewed as an extension of Barendregt's lambda cube.

### Study of type checking

- [eopl/lang_checked](src/main/scala/xieyuheng/eopl/lang_checked) <br>
  - A type-checked Language derived from `lang_letrec`.

### Study of type inference

- **TODO** [eopl/lang_infered](src/main/scala/xieyuheng/eopl/lang_infered) <br>

- **TODO** [miniml](src/main/scala/xieyuheng/miniml) <br>
  - "A simple applicative language: Mini-ML", <br>
    by Dominique Clement, Joelle Despeyroux, Thierry Despeyroux, Gilles Kahn.

### Study of module system

- **TODO** [eopl/lang_module](src/main/scala/xieyuheng/eopl/lang_module) <br>

### Study of object and subtyping

- **TODO** [eopl/lang_class](src/main/scala/xieyuheng/eopl/lang_class) <br>

### Study of normalization by evaluation (a.k.a. nbe)

- [nbe/lambda](src/main/scala/xieyuheng/nbe/lambda) <br>
  - The untyped lambda calculus.
- [nbe/syst](src/main/scala/xieyuheng/nbe/syst) <br>
  - Kurt Gödel's system T.
  - Simply typed lambda calculus with natural number.
- [nbe/tartlet](src/main/scala/xieyuheng/nbe/tartlet) <br>
  - "Checking Dependent Types with Normalization by Evaluation: A Tutorial", <br>
    by David Thrane Christiansen ([the original tutorial](http://davidchristiansen.dk/tutorials/nbe)).
  - Since tartlet is little pie, recursion is also not an option here.
- [nbe/minitt](src/main/scala/xieyuheng/nbe/minitt) <br>
  - "A simple type-theoretic language: Mini-TT", <br>
    by Thierry Coquand, Yoshiki Kinoshita, Bengt Nordström, Makoto Takeyama. <br>
  - No termination check, thus when viewed as logic it is unsound.

### Study of function composition and stack machine

- [adventure/jojo_untyped](src/main/scala/xieyuheng/adventure/jojo_untyped) <br>
  - De Bruijn notation extended by function composition, <br>
    implemented by stack machine and threaded code techniques, <br>
    which is an implementation techniques commonly used for
    [Forth (programming language)](https://en.wikipedia.org/wiki/Forth_(programming_language)).
- [adventure/jojo_simple](src/main/scala/xieyuheng/adventure/jojo_simple) <br>
  - Two levels of computations -- `exe` and `cut`, where `cut` is simple.
  - "Simply Typed JoJo Calculus", by Xie Yuheng ([the paper](docs/paper/simply-typed-jojo-calculus.md)).
- **TODO** [adventure/jojo_dependent](src/main/scala/xieyuheng/adventure/jojo_dependent) <br>
  - Two levels of computations -- `exe` and `cut`, where `cut` is as powerful as `exe`.

### references

- "Parsing Techniques -- A Practical Guide, Second Edition", <br>
  by Dick Grune and Ceriel J.H. Jacobs.
- "Essentials of Programming Languages (a.k.a. EOPL)", <br>
  by Daniel P. Friedman and Mitchell Wand.
- "Lambda Calculus with Types", <br>
  by Henk Barendregt, Wil Dekkers and Richard Statman.

## Usage

- Build: `sbt stage`
  - Then you can find executable files in `./target/universal/stage/bin/`
- Test: `./dev t`
  - [Bug report](https://github.com/xieyuheng/cicada/issues)
- Example code at: [example](example)

## Community

- We enforce C4 as collaboration protocol.
  - [The C4 RFC](https://rfc.zeromq.org/spec:42/C4)
- [Style Guide](STYLE-GUIDE.md)
  - Observe the style of existing code and respect it.
- [Code of Conduct](CODE-OF-CONDUCT.md)
- [Travis CI](https://travis-ci.org/xieyuheng/cicada)

## License

- [GPLv3](LICENSE)
