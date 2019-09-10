module algebra

class monoid_t extends semigroup_t {
  id: E
  left_id(x: E): eqv(mul(id, x), x)
  right_id(x: E): eqv(mul(x, id), x)

  as_category = category_t(
    unit_t,
    (_: unit_t, _: unit_t) => E,
    (_: unit_t): E => id,
    mul,
    left_id,
    left_id,
    associative,
  )
}
