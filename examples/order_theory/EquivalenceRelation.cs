module order_theory

class EquivalenceRelation <: PreOrder {
  Equivalence[~](a: E, b: E): Type = a <= b
  symmetric(a ~ b): b ~ a
}
