module order

class EquivalenceRelation extends PreOrder {
  @infix(~) Equivalence(a: E, b: E): Type =
    a <= b

  symmetric(a ~ b): b ~ a
}
