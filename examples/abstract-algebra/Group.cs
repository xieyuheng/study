module abstract-algebra

class Group <: Monoid {
  inv(x: E): id * x == x
  left_inv(x: E): inv(x) * x == id
  right_inv(x: E): x * inv(x) == id

  (/)(x: E, y: E): E = x * inv(y)
}
