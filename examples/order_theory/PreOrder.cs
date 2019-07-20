module order_theory

class PreOrder {
  E: Type
  Pre[<=](E, E): Type
  reflexive(a: E): a <= a
  transitive(a <= b, b <= c): a <= c

  StrictPre[<](a: E, b: E): Type =
    (a <= b, a != b)
}

// thin category
// homSet(A, B).size <= 1
