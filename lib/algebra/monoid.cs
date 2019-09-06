module algebra

class monoid_t extends semigroup_t {
  id: E
  left_id(x: E): id * x == x
  right_id(x: E): x * id == x
}

extend monoid_t {
  as_category = category_t {
    object_t = Unit
    morphism_t(_: Unit, _: Unit) = E

    id(_: Unit): E = this.id

    then = mul

    left_id = this.left_id
    right_id = this.left_id

    associative = this.associative
  }
}
