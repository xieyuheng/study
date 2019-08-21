module algebra

class category_t {
  object_t: type_t

  @infix(->) morphism_t(object_t, object_t): type_t

  id(a: object_t): a -> a

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

  class monomorphism_t {
    mono: a -> b
    right_cancelable(f, g: c -> a, f | mono == g | mono): f == g
  }

  class epimorphism_t {
    epi: a -> b
    left_cancelable(f, g: b -> c, epi | f  == epi | g): f == g
  }

  left_inverse_t(f: a -> b, g: a -> b): type_t =
    f | g == id(a)

  right_inverse_t(f: a -> b, g: a -> b): type_t =
    g | f == id(b)

  class isomorphism_t {
    iso: a -> b
    inv: b -> a
    left_inverse: left_inverse_t(iso, inv)
    right_inverse: right_inverse_t(iso, inv)
  }
}
