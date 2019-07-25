module category_theory

class Category {
  Object: Type
  Morphism[->](Object, Object): Type

  id(a: Object): a -> a

  @infix(|) compose(f: a -> b, g: b -> c): b -> c

  left_id(f: a -> b): id(a) | f == f
  right_id(f: a -> b): f | id(b) == f

  associative(
    f: a -> b,
    g: b -> c,
    h: c -> d,
  ): f | (g | h) == (f | g) | h

  /** derived types: */

  Inverse(f: a -> b, g: a -> b): Type =
    (f | g == id(a), g | f == id(b))

  class Isomorphism {
    iso: a -> b
    inv: b -> a
    inverse: Inverse(iso, inv)
  }
}
