module order

class partial_order_t extends pre_order_t {
  antisymmetric(pre_t(a, b), pre_t(b, a)): eqv(a, b)

  cover_t(a: E, b: E): type_t =
    (strict_pre_t(a, b),
      (x: E, pre_t(a, x), strict_pre_t(x, b)) -> eqv(x, a))
}

// no cycle
// topological sort
