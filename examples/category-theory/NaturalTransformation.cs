module category-theory

class NaturalTransformation {
  C: Category
  D: Category

  F: Functor(C, D)
  G: Functor(C, D)

  component(a: C.Object): D.Morphism(F.map(a), G.map(a))

  natural(
    f: F.Morphism(a, b)
  ): component(a) D.| G.fmap(f) == F.fmap(f) D.| component(b)

  note {
    component(a) : D.Morphism(F.map(a), G.map(a))
    G.fmap(f)    : D.Morphism(G.map(a), G.map(b))

    F.fmap(f)    : D.Morphism(F.map(a), F.map(b))
    component(b) : D.Morphism(F.map(b), G.map(b))
  }
}
