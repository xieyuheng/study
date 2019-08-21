module algebra

class functor_t {
  C: category_t
  D: category_t

  map(a: C.object_t): D.object_t
  fmap(f: a C.-> b): map(a) D.-> map(b)

  fmap_respect_then(
    f: a C.-> b,
    g: b C.-> c,
  ): fmap(f C.| g) == fmap(f) D.| fmap(g)

  fmap_respect_id(
    a: C.object_t
  ): fmap(C.id(a)) == D.id(map(a))
}
