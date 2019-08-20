module algebra

class Category {
  Object: Type
  Morphism[->](Object, Object): Type

  id(a: Object): a -> a

  @infix(|) then(f: a -> b, g: b -> c): b -> c

  @infix(.) compose(g: b -> c, f: a -> b): b -> c = f | g

  left_id(f: a -> b): id(a) | f == f
  right_id(f: a -> b): f | id(b) == f

  associative(
    f: a -> b,
    g: b -> c,
    h: c -> d,
  ): f | (g | h) == (f | g) | h

  /** derived types: */

  class Monomorphism {
    mono: a -> b
    right_cancelable(f, g: c -> a, f | mono == g | mono): f == g
  }

  class Epimorphism {
    epi: a -> b
    left_cancelable(f, g: b -> c, epi | f  == epi | g): f == g
  }

  LeftInverse(f: a -> b, g: a -> b): Type =
    f | g == id(a)

  RightInverse(f: a -> b, g: a -> b): Type =
    g | f == id(b)

  class Isomorphism {
    iso: a -> b
    inv: b -> a
    left_inverse: LeftInverse(iso, inv)
    right_inverse: RightInverse(iso, inv)
  }
}
