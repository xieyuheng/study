package xieyuheng.structure

abstract class group_t extends monoid_t {
  def inv(x: elem_t): elem_t

  def div(x: elem_t, y: elem_t): elem_t =
    mul(x, inv(y))

  def id_inv(x: elem_t) = {
    eqv(mul(x, inv(x)), id)
  }
}
