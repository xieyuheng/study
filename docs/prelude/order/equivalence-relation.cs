module order

class EquivalenceRelation extends PreOrder {
  @infix(~) Equivalence(a: E, b: E): type_t =
    a <= b

  symmetric(a ~ b): b ~ a
}
