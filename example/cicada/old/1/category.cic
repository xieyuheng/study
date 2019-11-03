class category_t {
  object_t: type
  arrow_t: type

  dom: (f: arrow_t) -> object_t
  cod: (f: arrow_t) -> object_t

  id: (x: object_t) -> arrow_t

  compose: (
    f: arrow_t,
    g: arrow_t,
    [implicit]: {
      composable: eqv_t (cod (f), dom (g)),
    }
  ) -> {
    return h: arrow_t
    eqv_dom: eqv_t (dom (h), dom (f))
    eqv_cod: eqv_t (cod (h), cod (g))
  }

  id_left: (
    f: arrow_t,
  ) -> eqv_t (compose (id (f.dom), f), f)

  id_right: (
    f: arrow_t,
  ) -> eqv_t (compose (f, id (f.cod)), f)

  assoc: (
    f: arrow_t,
    g: arrow_t,
    h: arrow_t,
  ) -> eqv_t (
    compose (f, compose (g, h)),
    compose (compose (f, g), h),
  )
}

class iso_t {
  cat: category_t
  iso: cat.arrow_t
  inv: cat.arrow_t

  iso_inv_identity: eqv_t (cat.compose (iso, inv), cat.id (a))
  inv_iso_identity: eqv_t (cat.compose (inv, iso), cat.id (b))
}

class functor_t {
  lcat: category_t
  rcat: category_t

  map: (x: lcat.object_t) -> rcat.object_t

  fmap: (f: lcat.arrow_t) -> {
    return g: rcat.arrow_t
    eqv_dom: eqv_t (map (lcat.dom (f)), rcat.dom (g))
    eqv_cod: eqv_t (map (lcat.cod (f)), rcat.cod (g))
  }

  fmap_respect_compose: (
    f: lcat.arrow_t,
    g: lcat.arrow_t,
  ) -> eqv_t (
    fmap (lcat.compose (f, g)),
    rcat.compose (fmap (f), fmap (g)),
  )

  fmap_respect_id: (
    x: lcat.object_t,
  ) -> eqv_t (
    fmap (lcat.id (x)),
    rcat.id (map (x)),
  )
}

class natural_transformation_t {
  lfun: functor_t
  rfun: functor_t

  lcat = lfun.lcat
  rcat = lfun.rcat

  lcat = rfun.lcat
  rcat = rfun.rcat

  component: (x: lcat.object_t) -> {
    return c: rcat.arrow_t
    eqv_dom: eqv_t (rcat.dom (c), lfun.map (x))
    eqv_cod: eqv_t (rcat.cod (c), rfun.map (x))
  }

  natural: (
    f: lcat.arrow_t
  ) -> eqv_t (
    rcat.compose (
      component (lcat.dom (f)),
      rfun.fmap (f),
    ),
    rcat.compose (
      lfun.fmap (f),
      component (lcat.cod (f)),
    ),
  )
}

class groupoid_t extends category_t {
  inv: (f: arrow_t) -> arrow_t

  arrow_iso: (f: arrow_t) -> iso_t (
    cat = this,
    iso = f,
    inv = inv (f),
  )
}
