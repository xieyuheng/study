package xieyuheng.untyped

import scala.annotation.tailrec

object util {
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
