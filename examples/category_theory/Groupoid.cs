module category_theory

class Groupoid extends Category {
  inv(f: a -> b): b -> a

  isomorphic_inv(f: a -> b): Isomorphism(f, inv(f))
}
