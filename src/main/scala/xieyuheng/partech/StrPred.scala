package xieyuheng.partech

case class StrPred
  (name: String, length: Int)
  (pred: String => Boolean) {

  override def toString = {
    s"[${name}#${length}?]"
  }

  def check(str: String): Boolean = {
    str.length >= length && pred(str.take(length))
  }
}
