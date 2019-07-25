module abstract_algebra

class Semigroup {
  E: Type
  @infix(*) mul(x: E, y: E): E
  associative(x: E, y: E, z: E): x * (y * z) == (x * y) * z
}
