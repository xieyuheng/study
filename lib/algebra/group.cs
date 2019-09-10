module algebra

class group_t extends monoid_t {
  inv(x: E): eqv(mul(id, x), x)
  left_inv(x: E): eqv(mul(inv(x), x), id)
  right_inv(x: E): eqv(mul(x, inv(x)), id)

  div(x: E, y: E): E = mul(x, inv(y))
}

class group_hom_t {
  G: group_t
  H: group_t

  hom(G.E): H.E
  hom_respect_mul(x: G.E, y: G.E): eqv(
    hom(G.mul(x, y)),
    H.mul(hom(x), hom(y)))
}

id_group_hom(G: group_t) = group_hom_t(
  G, G,
  (x: G.E) => x,
  (x: G.E, y: G.E) => same(G.mul(x, y)),
)

group_category = category_t(
  group_t,
  group_hom_t,
  id_group_hom,

  (f: group_hom_t(G, H), g: group_hom_t(H, K)) => group_hom_t(
    G, K,
    (x: G.E) => g.hom(f.hom(x)),
    hom_respect_mul(x, y): eqv(
      g.hom(f.hom(G.mul(x, y))), K.mul(g.hom(f.hom(x)), g.hom(f.hom(y)))
    ) => same(g.hom(H.mul(f.hom(x), f.hom(y))))
  ),

  (f: group_hom_t(G, H)): eqv(compose(id_group_hom(G), f), f) => same(f),
  (f: group_hom_t(G, H)): eqv(compose(f, id_group_hom)(H), f) => same(f),

  (f: group_hom_t(a, b),
    g: group_hom_t(b, c),
    h: group_hom_t(c, d),
  ): eqv(
    compose(f, compose(g, h)),
    compose(compose(f, g), h),
  ) => refl
)
