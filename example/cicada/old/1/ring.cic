import {
  abelian_group_t,
  monoid_t,
} from "./group.cs"

class ring_t {
  element_t: type

  addition: abelian_group_t (
    element_t = element_t
  )

  multiplication: monoid_t (
    element_t = element_t
  )

  zero = addition.id
  add = addition.add
  neg = addition.neg
  sub = addition.sub

  one = multiplication.id
  mul = multiplication.mul

  left_distr: (
    x: element_t,
    y: element_t,
    z: element_t,
  ) -> eqv_t (
    mul (x, add (y, z)),
    add (mul (x, y), mul (x, z)),
  )

  right_distr: (
    x: element_t,
    y: element_t,
    z: element_t,
  ) -> eqv_t (
    mul (add (y, z), x),
    add (mul (y, x), mul (z, x)),
  )
}

export
class commutative_ring_t extends ring_t {
  commu: (
    x: element_t,
    y: element_t,
  ) -> eqv_t (
    mul (x, y),
    mul (y, x),
  )

  distr = left_distr
}

class integral_ring_t extends commutative_ring_t {
  nonzero_product: (
    x: element_t, not_eqv_t (x, zero),
    y: element_t, not_eqv_t (y, zero),
  ) -> not_eqv_t (mul (x, y), zero)
}

class euclidean_ring_t extends integral_ring_t {
  degree: (x: element_t) -> number

  // TODO
}

class field_t extends integral_ring_t {
  inv: (x: element_t) -> element_t

  div: (
    x: element_t,
    y: element_t,
  ) -> element_t = {
    mul (x, inv (y))
  }
}
