module order_theory

class EquivalenceRelation extends PreOrder {
  Equivalence[~](a: E, b: E): Type = a <= b
  symmetric(a ~ b): b ~ a
}
