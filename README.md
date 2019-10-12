# cicada

programming language implementation practices

## Techniques

> Remember,<br>
> an engineer solve problem,<br>
> an engineer does not want just a religion about how to solve a problem.<br>
> -- Gerry Sussman

### parsing techniques

- [partech](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/partech):
  A parser generator, with various parsing techniques
  - [Earley parser](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/partech/parsing_techniques/Earley.scala)
    - O(n^3) for general context free grammar

### interpreting techniques

- [eopl](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl):
  Essentials of Programming Languages
  - by Daniel P. Friedman and Mitchell Wand
  - study of the binding and scoping of variables:
    - [lang_let](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_let):
      simple scope
    - [lang_proc](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_proc):
      closure
    - [lang_letrec](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_letrec):
      recursive closure
    - [lang_nameless](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_nameless):
      de bruijn index
  - study of assignment to a location in memory (most basic kind of effect):
    - [lang_explicit_refs](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_explicit_refs):
      explicit reference (address)
    - [lang_implicit_refs](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_implicit_refs):
      distinguish value from denoted value, distinguish left value and right value (like c).
    - [lang_mutable_pairs](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_mutable_pairs):
      remember Dan's paper "Cons should not evaluate its arguments" ? this is preparation for the paper.
    - [lang_lazy](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_lazy):
      remember Dan's paper "Cons should not evaluate its arguments" ? this is about the paper.

### type checking techniques

- [lambda](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/lambda):
  The untyped lambda calculus
  - with norm-by-eval (normalization by evaluation).
- [syst](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/syst):
  Kurt Gödel's system T
  - simply typed lambda calculus with natural number,
  - for details about system T, see "Gödel’s system T as a precursor of modern type theory", by Gilles Dowek
  - with typed norm-by-eval (a step toward tartlet and pie).
- [tartlet](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/tartlet):
  Little pie
  - recursion is not an option.
  - from the tutorial "Checking Dependent Types with Normalization by Evaluation: A Tutorial",
    by David Thrane Christiansen.
    ([the original tutorial](http://davidchristiansen.dk/tutorials/nbe))
- [minitt](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/minitt):
  The dependently typed Mini-TT language
  - described in paper: "A simple type-theoretic language: Mini-TT",
    by Thierry Coquand, Yoshiki Kinoshita, Bengt Nordström, Makoto Takeyama.
  - no termination check, thus when viewed as logic it is unsound.
- [de_bruijn](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/de_bruijn):
  De Bruijn stype simple type lambda calculus
  - implemented by term rewriting.

### bizarre techniques

- [adventure](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/adventure):
  - [untyped](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/adventure/untyped):
    de bruijn notation implemented by threaded code
  - [simple](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/adventure/simple):
    two levels of computations -- `exe` and `cut`
    - `exe` level computation is just computation
    - `cut` level computation is type checking
    - `let` is not allowed in `cut` level computation
  - [dependent](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/adventure/dependent):
    the same as `simple`, but allow `let` in `cut`

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
