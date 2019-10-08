package xieyuheng.eopl.lang_nameless

case class IdxCtx(list: List[String]) {

  def lookup_index(name: String): Option[Int] = {
    val index = list.indexOf(name)
    if (index == -1) {
      None
    } else {
      Some(index)
    }
  }

  def ext_let(name: String): IdxCtx = {
    IdxCtx(name +: list)
  }

}
