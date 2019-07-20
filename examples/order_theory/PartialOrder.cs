module order_theory

class PartialOrder <: PreOrder {
  antisymmetric(a <= b, b <= a): a == b
}

// no cycle
// topological sort
