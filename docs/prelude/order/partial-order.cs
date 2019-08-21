module order

class partial_order_t extends pre_order_t {
  antisymmetric(a <= b, b <= a): a == b

  @infix(-<) cover_t(a: E, b: E): type_t =
    (a < b, (x: E, a <= x < b) => x == a)
}

// no cycle
// topological sort
