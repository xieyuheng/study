module order

class PartialOrder extends PreOrder {
  antisymmetric(a <= b, b <= a): a == b

  @infix(-<) Cover(a: E, b: E): Type =
    (a < b, (x: E, a <= x < b) => x == a)
}

// no cycle
// topological sort
