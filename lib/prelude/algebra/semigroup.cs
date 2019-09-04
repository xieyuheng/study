module algebra

class semigroup_t {
  E: type_t
  @infix(*) mul(x: E, y: E): E
  associative(x: E, y: E, z: E): x * (y * z) == (x * y) * z
}
