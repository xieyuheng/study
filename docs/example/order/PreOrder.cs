module order

class PreOrder {
  E: type_t
  @infix(<=) Pre(E, E): type_t
  reflexive(a: E): a <= a
  transitive(a <= b, b <= c): a <= c

  @infix(<) StrictPre(a: E, b: E): type_t =
    (a <= b, a != b)
}

// thin category
// homSet(A, B).size <= 1
