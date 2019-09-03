package xieyuheng.partech

case class StrPred
  (name: String, length: Int, gen: () => String)
  (pred: String => Boolean) {

  assert(length > 0)

  override def toString = {
    s"[${length}#${name}]"
  }

  def check(str: String): Boolean = {
    str.length >= length && pred(str.take(length))
  }
}
