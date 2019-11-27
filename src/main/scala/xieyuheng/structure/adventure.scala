package xieyuheng.structure

abstract class adventure_t {
  type elem_t

  def cut(x: elem_t): elem_t

  def mul(f: elem_t, g: elem_t): elem_t

  def mul_cut(f: elem_t, g: elem_t) = {
    eqv(cut(mul(f, g)), mul(cut(f), cut(g)))
  }

  def mul_associative(f: elem_t, g: elem_t, h: elem_t) = {
    eqv(mul(f, mul(g, h)), mul(mul(f, g), h))
  }

  def id: elem_t

  def id_left(f: elem_t) = {
    eqv(f, mul(id, f))
  }

  def id_rigth(f: elem_t) = {
    eqv(f, mul(f, id))
  }

  def id_cut() = {
    assert(cut(id) == id)
  }

  def error: elem_t

  def error_left(f: elem_t) = {
    eqv(error, mul(error, f))
  }

  def error_right(f: elem_t) = {
    eqv(error, mul(f, error))
  }

  def error_cut() = {
    assert(cut(error) == error)
  }
}
