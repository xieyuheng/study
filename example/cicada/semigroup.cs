class semigroup_t {
  E : type
  mul : (x: E, y: E) -> E
  associative : (x: E, y: E, z: E) ->
    eqv_t(E, mul(x, mul(y, z)), mul(mul(x, y), z))
}

semigroup : semigroup_t
semigroup = semigroup_t(E, mul, associative)
