class eqv_t {
  [implicit]: { t: type }
  lhs: t
  rhs: t
  /**
   * `the_same` for normalization by evaluation with neutral expressions
   */
  the_same (lhs, rhs)
}

eqv_apply: (
  [implicit]: { a: type, b: type }
  fun : (a) -> b,
  [implicit]: { x: a, y: a }
  eqv_t (x, y),
) -> eqv_t (fun (x), fun (y)) = eqv_t ()

eqv_swap: (
  [implicit]: { a: type }
  [implicit]: { x: a, y: a }
  eqv_t (x, y)
) -> eqv_t (y, x) = eqv_t ()

eqv_compose: (
  [implicit]: { a: type }
  [implicit]: { x: a, y: a, z: a }
  eqv_t (x, y),
  eqv_t (y, z),
) -> eqv_t (x, z) = eqv_t ()
