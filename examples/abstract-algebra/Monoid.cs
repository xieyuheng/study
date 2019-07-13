module abstract-algebra

import category-theory._

class Monoid <: Semigroup {
  id: E
  left_id(x: E): id * x == x
  right_id(x: E): x * id == x
}

extend Monoid {
  asCategory = Category {
    Object = Unit
    Morphism(_: Unit, _: Unit) = E

    id(_: Unit): E = this.id

    compose = mul

    left_id = this.left_id
    right_id = this.left_id

    associative = this.associative
  }
}
