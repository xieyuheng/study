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

## Docs

- [Fulfilling Type System (paper)](docs/fulfilling-type-system.md)
