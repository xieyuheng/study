module algebra

class Semigroup {
  E: type_t
  @infix(*) mul(x: E, y: E): E
  associative(x: E, y: E, z: E): x * (y * z) == (x * y) * z
}
