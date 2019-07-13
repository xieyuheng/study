module category-theory

class Groupoid <: Category {
  inv(f: Arrow(a, b)): Arrow(b, a)

  isomorphic_inv(f: Arrow(a, b)): Isomorphism(f, inv(f))
}
