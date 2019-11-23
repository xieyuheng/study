package xieyuheng.structure

abstract class adventure_t {
  type jojo_t

  def cut(x: jojo_t): jojo_t

  def mul(f: jojo_t, g: jojo_t): jojo_t

  def cut_respect_mul(f: jojo_t, g: jojo_t) = {
    eqv(cut(mul(f, g)), mul(cut(f), cut(g)))
  }

  def mul_associative(f: jojo_t, g: jojo_t, h: jojo_t) = {
    eqv(mul(f, mul(g, h)), mul(mul(f, g), h))
  }

  def id: jojo_t

  def id_left(f: jojo_t) = {
    eqv(f, mul(id, f))
  }

  def id_rigth(f: jojo_t) = {
    eqv(f, mul(f, id))
  }

  def error: jojo_t

  def error_left(f: jojo_t) = {
    eqv(error, mul(error, f))
  }

  def error_right(f: jojo_t) = {
    eqv(error, mul(f, error))
  }
}
