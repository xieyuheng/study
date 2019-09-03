package xieyuheng.partech

import java.util.UUID

case class StrPred(
  length: Int,
  pred: String => Boolean,
  description: String = UUID.randomUUID().toString,
) {
  override def toString = {
    s"[${description}#${length}?]"
  }

  def check(str: String): Boolean = {
    str.length <= length && pred(str.take(length))
  }
}
