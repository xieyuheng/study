module algebra

class natural_transformation_t {
  C: category_t
  D: category_t

  F: functor_t(C, D)
  G: functor_t(C, D)

  component(a: C.object_t): F.map(a) D.-> G.map(a)

  natural(
    f: F.morphism_t(a, b)
  ): component(a) D.| G.fmap(f) == F.fmap(f) D.| component(b)

  note {
    component(a) : F.map(a) D.-> G.map(a)
    G.fmap(f)    : G.map(a) D.-> G.map(b)

    F.fmap(f)    : F.map(a) D.-> F.map(b)
    component(b) : F.map(b) D.-> G.map(b)
  }
}
