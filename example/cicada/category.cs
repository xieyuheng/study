class category_t {

  object_t : type

  morphism_t : (object_t, object_t) -> type

  id : (a: object_t) -> morphism_t(a, a)

  compose : [a: object_t, b: object_t, c: object_t] -> (
    f: morphism_t(a, b),
    g: morphism_t(b, c),
  ) -> morphism_t(a, c)

  left_id : (f: morphism_t(a, b)) -> eqv_t(compose(id(a), f), f)
  right_id : (f: morphism_t(a, b)) -> eqv_t(compose(f, id(b)), f)

  associative : [a: object_t, b: object_t, c: object_t, d: object_t] -> (
    f: morphism_t(a, b),
    g: morphism_t(b, c),
    h: morphism_t(c, d),
  ) -> eqv_t(compose(f, compose(g, h)), compose(compose(f, g), h))


  class monomorphism_t {
    mono : [a: object_t, b: object_t] -> morphism_t(a, b)
    right_cancelable : [a: object_t, b: object_t, c: object_t] -> (
      f: morphism_t(c, a),
      g: morphism_t(c, a),
      eqv_t(compose(f, mono), compose(g, mono)),
    ) -> eqv_t(f, g)
  }

  class epimorphism_t {
    epi : [a: object_t, b: object_t] -> morphism_t(a, b)
    left_cancelable : [a: object_t, b: object_t, c: object_t] -> (
      f: morphism_t(b, c),
      g: morphism_t(b, c),
      eqv_t(compose(epi, f), compose(epi, g)),
    ) -> eqv_t(f, g)
  }

  left_inverse_t : [a: object_t, b: object_t] -> (f: morphism_t(a, b), g: morphism_t(a, b)) -> type
  left_inverse_t = [a: object_t, b: object_t] -> (f: morphism_t(a, b), g: morphism_t(a, b)) => {
    eqv_t(compose(f, g), id(a))
  }

  right_inverse_t : [a: object_t, b: object_t] -> (f: morphism_t(a, b), g: morphism_t(a, b)) -> type
  right_inverse_t = [a: object_t, b: object_t] -> (f: morphism_t(a, b), g: morphism_t(a, b)) => {
    eqv_t(compose(g, f), id(b))
  }

  class isomorphism_t {
    iso : [a: object_t, b: object_t] -> morphism_t(a, b)
    inv : [a: object_t, b: object_t] -> morphism_t(b, a)
    left_inverse : left_inverse_t(iso, inv)
    right_inverse : right_inverse_t(iso, inv)
  }

}
