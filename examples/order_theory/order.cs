module order_theory

// thin category
// homSet(A, B).size <= 1
class PreOrder {
  E: Type
  Pre[<=](E, E): Type
  reflexive(a: E): a <= a
  transitive(a <= b, b <= c): a <= c
}

// no cycle
// topological sort
class PartialOrder <: PreOrder {
  Eqv[==](E, E): Type
  antisymmetric(a <= b, b <= a): a == b
}

class EqvRelation <: PreOrder {
  symmetric(a <= b): b <= a
}

// quick sort
// bubble sort
// merge sort
class TotalOrder <: PartialOrder {
  connex(a: E, b: E): Either[a <= b, b <= a]
}
