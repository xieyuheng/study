module category_theory

class Category {
  Object: Type
  Morphism(Object, Object): Type

  id(a: Object): Morphism(a, a)

  compose[|](f: Morphism(a, b), g: Morphism(b, c)): Morphism(b, c)

  left_id(f: Morphism(a, b)): id(a) | f == f
  right_id(f: Morphism(a, b)): f | id(b) == f

  associative(
    f: Morphism(a, b),
    g: Morphism(b, c),
    h: Morphism(c, d),
  ): f | (g | h) == (f | g) | h

  /** derived types: */

  Inverse(f: Morphism(a, b), g: Morphism(a, b)): Type =
    (f | g == id(a), g | f == id(b))

  class Isomorphism {
    iso: Morphism(a, b)
    inv: Morphism(b, a)
    inverse: Inverse(iso, inv)
  }
}
