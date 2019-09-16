module algebra

class category_t {
  object_t: type_t

  morphism_t(object_t, object_t): type_t

  id(a: object_t): morphism_t(a, a)

  compose(f: morphism_t(a, b), g: morphism_t(b, c)): morphism_t(a, c)

  left_id(f: morphism_t(a, b)): eqv(compose(id(a), f), f)
  right_id(f: morphism_t(a, b)): eqv(compose(f, id(b)), f)

  associative(
    f: morphism_t(a, b),
    g: morphism_t(b, c),
    h: morphism_t(c, d),
  ): eqv(compose(f, compose(g, h)), compose(compose(f, g), h))

  /** derived types: */

  class monomorphism_t {
    mono: morphism_t(a, b)
    right_cancelable(
      f: morphism_t(c, a),
      g: morphism_t(c, a),
      eqv(compose(f, mono), compose(g, mono)),
    ): eqv(f, g)
  }

  class epimorphism_t {
    epi: morphism_t(a, b)
    left_cancelable(
      f: morphism_t(b, c),
      g: morphism_t(b, c),
      eqv(compose(epi, f), compose(epi, g)),
    ): eqv(f, g)
  }

  left_inverse_t(f: morphism_t(a, b), g: morphism_t(a, b)): type_t = {
    eqv(compose(f, g), id(a))
  }

  right_inverse_t(f: morphism_t(a, b), g: morphism_t(a, b)): type_t = {
    eqv(compose(g, f), id(b))
  }

  class isomorphism_t {
    iso: morphism_t(a, b)
    inv: morphism_t(b, a)
    left_inverse: left_inverse_t(iso, inv)
    right_inverse: right_inverse_t(iso, inv)
  }
}
