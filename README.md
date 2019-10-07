# cicada

programming language implementation practices.

## Techniques

### parsing techniques

- [partech](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/partech):
  An Earley parser generator

### interpreting techniques

- [eopl](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl):
  Essentials of Programming Languages
  - by Daniel P. Friedman and Mitchell Wand
  - study of the binding and scoping of variables:
    [lang_let](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/eopl/lang_let)
    [lang_proc]
    [lang_letrec]
    [lang_nameless]
  - study of assignment to a location in memory (most basic kind of effect):
    [lang_explicit_refs]
    [lang_implicit_refs]

### type checking techniques

- [lambda](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/lambda):
  The untyped lambda calculus
  - with norm-by-eval (normalization by evaluation).

- [syst](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/syst):
  Kurt Gödel's system T
  - simply typed lambda calculus with natural number,
  - for details about system T, see "Gödel’s system T as a precursor of modern type theory", by Gilles Dowek
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
    by Thierry Coquand, Yoshiki Kinoshita, Bengt Nordström, Makoto Takeyama.
  - no termination check, thus when viewed as logic it is unsound.

- [de_bruijn](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/de_bruijn):
  De Bruijn stype simple type lambda calculus
  - implemented by term rewriting.

## Usage

- Pre-compiled binaries at: [cicada-release](https://github.com/xieyuheng/cicada-release)
- Example code at: [example](https://github.com/xieyuheng/cicada/tree/master/example)

## Community

- We enforce C4 as collaboration protocol -- [The C4 RFC](https://rfc.zeromq.org/spec:42/C4)
- [Style Guide](STYLE-GUIDE.md) -- observe the style of existing code and respect it
- [Code of Conduct](CODE-OF-CONDUCT.md)
- [Travis CI](https://travis-ci.org/xieyuheng/cicada)

## License

- [GPLv3](LICENSE)
