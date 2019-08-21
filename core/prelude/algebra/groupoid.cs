module algebra

class groupoid_t extends category_t {
  inv(f: a -> b): b -> a

  isomorphic_inv(f: a -> b): Isomorphism(f, inv(f))
}
