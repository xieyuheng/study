module order_theory

class PartialOrder <: PreOrder {
  antisymmetric(a <= b, b <= a): a == b

  Cover[-<](a: E, b: E): Type =
    (a < b, (x: E, a <= x < b) => x == a)
}

// no cycle
// topological sort
