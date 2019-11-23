package xieyuheng.structure

abstract class semigroup_t {
  type elem_t

  def mul(x: elem_t, y: elem_t): elem_t

  def mul_associative(x: elem_t, y: elem_t, z: elem_t) =
    eqv(
      mul(mul(x, y), z),
      mul(x, mul(y, z)))
}
