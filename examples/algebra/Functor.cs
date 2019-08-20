module algebra

class Functor {
  C: Category
  D: Category

  map(a: C.Object): D.Object
  fmap(f: a C.-> b): map(a) D.-> map(b)

  fmap_respect_then(
    f: a C.-> b,
    g: b C.-> c,
  ): fmap(f C.| g) == fmap(f) D.| fmap(g)

  fmap_respect_id(
    a: C.Object
  ): fmap(C.id(a)) == D.id(map(a))
}
