module abstract-algebra

class Monoid <: Semigroup {
  id: E
  left_id(x: E): id * x == x
  right_id(x: E): x * id == x
}
