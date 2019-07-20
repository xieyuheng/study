module order_theory

class PreOrder {
  E: Type
  Pre[<=](E, E): Type
  reflexive(a: E): a <= a
  transitive(a <= b, b <= c): a <= c
}

// thin category
// homSet(A, B).size <= 1
