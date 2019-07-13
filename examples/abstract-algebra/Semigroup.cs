module abstract-algebra

class Semigroup {
  E: Type
  mul[*](x: E, y: E): E
  associative(x: E, y: E, z: E): x * (y * z) == (x * y) * z
}
