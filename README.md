# Study

Study of language design and implementation.

The aim of this repository is to quickly make prototype for language design ideas.

This repository contains a series of prototype language implementations, <br>
and some general supporting libraries (such as parsing and command line interface).

> Remember, <br>
> an engineer solve problem, <br>
> an engineer does not want just a religion about how to solve a problem. <br>
> -- Gerry Sussman

### Study of parsing techniques

- [partech](src/main/scala/xieyuheng/partech) <br>
  A parser generator, with various parsing techniques.
  - [Earley parser](src/main/scala/xieyuheng/partech/parsing_techniques/Earley.scala) <br>
    Its time complexity is `O(n^3)` in general for context free grammar.

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

### Study of type checking

- [eopl/lang_checked](src/main/scala/xieyuheng/eopl/lang_checked) <br>
  - A type-checked language derived from `lang_letrec`.

### Study of type inference

- [eopl/lang_infered](src/main/scala/xieyuheng/eopl/lang_infered) <br>
  - A type-infered language derived from `lang_letrec`.

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
    which is commonly used for [Forth (programming language)](https://en.wikipedia.org/wiki/Forth_(programming_language)).
- [adventure/jojo_simple](src/main/scala/xieyuheng/adventure/jojo_simple) <br>
  - Two level of compositions -- `exe` and `cut`, where `cut` is simple.

### Study of Foundations of Mathematics

- [cicada](src/main/scala/xieyuheng/cicada) <br>
  - Dependent type + structural subtyping.
  - Uses `type in type`.
  - No totality checking.
  - For doing experiments in Foundations of Mathematics.

## References

- "Parsing Techniques -- A Practical Guide"
  - by Dick Grune and Ceriel J.H. Jacobs.
- "Essentials of Programming Languages" (a.k.a. EOPL)
  - by Daniel P. Friedman and Mitchell Wand.
- "Lambda Calculus with Types"
  - by Henk Barendregt, Wil Dekkers and Richard Statman. 2010.
- "Do-it-yourself Type Theory"
  - by Roland Backhouse, Paul Chisholm, Erik Saaman and Grant Malcolm, 1988.
- "Computation and Reasoning -- A Type Theory for Computer Science"
  - by Zhaohui Luo, 1994.

## Usage

- Build: `sbt stage`
  - Then you can find executable files in `./target/universal/stage/bin/`
- Test: `./dev t`
- Example code at: [example](example)

## Community

Contributions are welcome, see [current TODO list](TODO.md) for tasks. <br>
(Please add yourself to [the AUTHORS list](AUTHORS) if you made any contributions.)

- We enforce C4 as collaboration protocol.
  - [The C4 RFC](https://rfc.zeromq.org/spec:42/C4)
- [Style Guide](STYLE-GUIDE.md)
  - Observe the style of existing code and respect it.
- [Code of Conduct](CODE-OF-CONDUCT.md)
- [Travis CI](https://travis-ci.org/xieyuheng/study)

## License

- [GPLv3](LICENSE)
