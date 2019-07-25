module category_theory

class NaturalTransformation {
  C: Category
  D: Category

  F: Functor(C, D)
  G: Functor(C, D)

  component(a: C.Object): F.map(a) D.-> G.map(a)

  natural(
    f: F.Morphism(a, b)
  ): component(a) D.| G.fmap(f) == F.fmap(f) D.| component(b)

  note {
    component(a) : F.map(a) D.-> G.map(a)
    G.fmap(f)    : G.map(a) D.-> G.map(b)

    F.fmap(f)    : F.map(a) D.-> F.map(b)
    component(b) : F.map(b) D.-> G.map(b)
  }
}
