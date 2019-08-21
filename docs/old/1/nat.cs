union nat_t {
  zero_t
  succ_t
} {}

class zero_t {}

class succ_t {
  prev: nat_t
}

add: (
  x: nat_t,
  y: nat_t,
) -> nat_t = case (x) {
  zero_t => y
  succ_t => succ_t (add (x.prev, y))
}

mul: (
  x: nat_t,
  y: nat_t,
) -> nat_t = case (x) {
  zero_t => zero_t
  succ_t => add (y, (mul (x.prev, y)))
}

factorial: (
  x: nat_t,
) -> nat_t = case (x) {
  zero_t => succ_t (zero_t)
  succ_t => mul (x, (factorial (x.prev)))
}

even_p: (
  x: nat_t,
) -> bool_t = case (x) {
  zero_t => true_t
  succ_t => case (x) {
    zero_t => false_t
    succ_t => even_p (x.prev.prev)
  }
}

union nat_even_t {
  zero_even_t
  even_plus_two_even_t
} {
  n: nat_t
}

class zero_even_t {
  n: zero_t ()
}

class even_plus_two_even_t {
  n: succ_t (succ_t (m))
  m: nat_t
  prev: nat_even_t (m)
}

two_even: nat_even_t (succ_t (succ_t (zero_t))) = {
  even_plus_two_even_t (
    n = succ_t (succ_t (zero_t))
    m = zero_t
    prev = zero_even_t (zero_t)
  )
}

add_assoc: (
  x: nat_t,
  y: nat_t,
  z: nat_t,
) -> eqv_t (
  add (add (x, y), z),
  add (x, add (y, z)),
) = case (x) {
  zero_t => eqv_t
  succ_t => eqv_apply (
    succ_t, add_assoc (x.prev, y, z)
  )
}

add_commu: (
  x: nat_t,
  y: nat_t,
) -> eqv_t (
  add (x, y),
  add (y, x),
) = case (x) {
  zero_t => add_zero_commu (y)
  succ_t => eqv_compose (
    eqv_apply (succ_t, add_commu (x.prev, y)),
    add_succ_commu (y, x.prev),
  )
}

add_zero_commu: (
  x: nat_t
) -> eqv_t (
  add (zero_t, x),
  add (x, zero_t),
) = case (x) {
  zero_t => eqv_t
  succ_t => eqv_apply (
    succ_t, add_zero_commu (x.prev),
  )
}

add_succ_commu_1: (
  x: nat_t,
  y: nat_t,
) -> eqv_t (
  add (succ_t (x), y),
  succ_t (add (x, y)),
) = case (x) {
  zero_t => eqv_t
  succ_t => eqv_apply (
    succ_t, add_succ_commu_1 (x.prev, y),
  )
}

add_succ_commu_2: (
  x: nat_t,
  y: nat_t,
) -> eqv_t (
  add (y, succ_t (x)),
  succ_t (add (x, y)),
) = case (x) {
  zero_t => eqv_t
  succ_t => eqv_apply (
    succ_t, add_succ_commu_2 (x.prev, y),
  )
}
