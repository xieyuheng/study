module order

class pre_order_t {
  E: type_t
  @infix(<=) pre_t(E, E): type_t
  reflexive(a: E): a <= a
  transitive(a <= b, b <= c): a <= c

  @infix(<) strict_pre_t(a: E, b: E): type_t =
    (a <= b, a != b)
}

// thin category
// hom_set(A, B).size <= 1
