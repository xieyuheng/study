module category-theory

class Functor {
  C: Category
  D: Category

  map(a: C.Object): D.Object
  fmap(f: C.Arrow(a, b)): D.Arrow(map(a), map(b))

  fmap_respect_compose(
    f: C.Arrow(a, b),
    g: C.Arrow(b, c),
  ): fmap(f C.| g) == fmap(f) D.| fmap(g)

  fmap_respect_id(
    a: C.Object
  ): fmap(C.id(a)) == D.id(map(a))
}
