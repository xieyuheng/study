module order_theory

class Lattic extends PartialOrder {
  antisymmetric(a <= b, b <= a): a == b

  @infix(-<) Cover(a: E, b: E): Type =
    (a < b, (x: E, a <= x < b) => x == a)

  join(a: E, b: E): E
  meet(a: E, b: E): E

  // TODO

  // associative
  // commutative
  // idempotent
}
