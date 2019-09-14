module algebra

class semigroup_t {
  E: type_t
  mul(x: E, y: E): E
  associative(x: E, y: E, z: E): eqv(mul(x, mul(y, z)), mul(mul(x, y), z))
}
