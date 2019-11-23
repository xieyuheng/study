package xieyuheng.structure

object eqv {
  def apply[T](x: T, y: T) = {
    assert(x == y)
  }
}
