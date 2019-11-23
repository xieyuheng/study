package xieyuheng.structure

abstract class monoid_t extends semigroup_t {
  def id: elem_t

  def id_left(x: elem_t) = {
    eqv(mul(id, x), x)
  }

  def id_right(x: elem_t) = {
    eqv(mul(x, id), x)
  }
}
