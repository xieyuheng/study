package xieyuheng.systemt

import scala.annotation.tailrec

object util {
  @tailrec
  def freshen(
    used_names: Set[String],
    name: String,
  ): String = {
    if (used_names.contains(name)) {
      freshen(used_names, name + "*")
    } else {
      name
    }
  }
}
