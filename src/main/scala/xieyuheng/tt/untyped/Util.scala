package xieyuheng.tt.untyped

import scala.annotation.tailrec

object Util {
  @tailrec
  def freshen(
    usedNames: Set[String],
    name: String,
  ): String = {
    if (usedNames.contains(name)) {
      freshen(usedNames, name + "*")
    } else {
      name
    }
  }
}
