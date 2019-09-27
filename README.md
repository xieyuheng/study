# cicada

A dependently typed language and proof assistant.
- with **fulfilling type system**, in which partially fulfilled data can be used as type.
- translate to human readable javascript, and maintain all type information at runtime.
- literal programming support by markdown, to help writers write proofs for human.

## Examples

### vector with length in type

``` cicada
import * from "../basic/nat.cic"

datatype vec_t(A: type_t, length: nat_t) {
  case vec_null_t(
    A: type_t,
    length: nat_t = zero_t)
  case vec_cons_t(
    A: type_t,
    head: A,
    tail: vec_t(A),
    length: nat_t = succ_t(tail.length))
}

fn vec_append(
  A: type_t,
  x: vec_t(A),
  y: vec_t(A),
): vec_t(A, nat_add(x.length, y.length)) = {
  x choice {
    case vec_null_t => y
    case vec_cons_t => vec_cons_t(A, x.head, vec_append(A, x.tail, y))
  }
}

fn vec_map(A: type_t, B: type_t, f: (A) -> B, vec: vec_t(A)): vec_t(B, vec.length) = {
  vec choice {
    case vec_null_t => vec
    case vec_cons_t => vec_cons_t(nat_t, f(vec.head), vec_map(A, B, f, vec.tail))
  }
}
```

## Getting Start

- Pre-compiled binaries at: [cicada-release](https://github.com/xieyuheng/cicada-release)
- Example code at: [example](https://github.com/xieyuheng/cicada/tree/master/example)

## Also Contains

- [partech](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/partech):
  A parser generator with different parsing techniques.
- [lambda](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/lambda):
  The untyped lambda calculus
  - with norm-by-eval (normalization by evaluation).
- [syst](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/syst):
  Kurt Gödel's system T (curry style simply typed lambda calculus with natural number),
  - for details about system T, see "Gödel’s system T as a precursor of modern type theory", by Gilles Dowek
  - with typed norm-by-eval (a step toward tartlet and pie).
- [tartlet](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/tartlet)
  The tutorial "Checking Dependent Types with Normalization by Evaluation: A Tutorial",
  by David Thrane Christiansen.
  ([the original tutorial](http://davidchristiansen.dk/tutorials/nbe))
  - subrecursive, recursion is not an option.
- [minitt](https://github.com/xieyuheng/cicada/tree/master/src/main/scala/xieyuheng/minitt):
  The dependently typed Mini-TT language
  described in paper: "A simple type-theoretic language: Mini-TT",
  by Thierry Coquand, Yoshiki Kinoshita, Bengt Nordström, Makoto Takeyama.
  - no termination check, thus when viewed as logic it is unsound.

## Community

- We enforce C4 as collaboration protocol -- [The C4 RFC](https://rfc.zeromq.org/spec:42/C4)
- [Style Guide](STYLE-GUIDE.md) -- observe the style of existing code and respect it
- [Code of Conduct](CODE-OF-CONDUCT.md)
- [Travis CI](https://travis-ci.org/xieyuheng/cicada)

## License

- [GPLv3](LICENSE)
