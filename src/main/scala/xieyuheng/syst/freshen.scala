package xieyuheng.syst

import scala.annotation.tailrec

object freshen {

  @tailrec
  def apply(used_names: Set[String], name: String): String = {
    if (used_names.contains(name)) {
      freshen(used_names, name + "*")
    } else {
      name
    }
  }

}
