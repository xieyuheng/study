module algebra

class group_t extends monoid_t {
  inv(x: E): id * x == x
  left_inv(x: E): inv(x) * x == id
  right_inv(x: E): x * inv(x) == id

  @infix(/) div(x: E, y: E): E = x * inv(y)
}

class group_hom_t {
  G: group_t
  H: group_t

  hom(G.E): H.E
  hom_respect_mul(x: G.E, y: G.E): hom(x G.* y) == hom(x) H.* hom(y)
}

id_group_hom(G: group_t) = group_hom_t {
  G, G
  hom(x) = x
  hom_respect_mul(x, y) = same(x G.* y)
}

group_category = category_t {
  object_t = group_t
  morphism_t = group_hom_t
  id = id_group_hom

  then(f: group_hom_t(G, H), g: group_hom_t(H, K)) = group_hom_t {
    G, K
    hom(x) = g.hom(f.hom(x))
    hom_respect_mul(x, y): g.hom(f.hom(x G.* y)) == g.hom(f.hom(x)) K.* g.hom(f.hom(y)) =
      same(g.hom(f.hom(x* y) H.* f.hom(y)))
  }

  left_id(f: group_hom_t(G, H)): id_group_hom(G) | f == f =
    same(f)
  right_id(f: group_hom_t(G, H)): f | id_group_hom(H) == f =
    same(f)

  associative(
    f: group_hom_t(a, b),
    g: group_hom_t(b, c),
    h: group_hom_t(c, d),
  ): f | (g | h) == (f | g) | h = refl
}
