module order

class equivalence_relation_t extends pre_order_t {
  @infix(~) equivalence_t(a: E, b: E): type_t =
    a <= b

  symmetric(a ~ b): b ~ a
}
