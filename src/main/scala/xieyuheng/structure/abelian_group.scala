package xieyuheng.structure

abstract class abelian_group_t extends group_t {
  def add(x: elem_t, y: elem_t): elem_t = mul(x, y)
  def sub(x: elem_t, y: elem_t): elem_t = div(x, y)
  def neg(x: elem_t): elem_t = inv(x)

  def add_commutative(x: elem_t, y: elem_t) = {
    eqv(add (x, y), add (y, x))
  }
}
