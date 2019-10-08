package xieyuheng.eopl.lang_nameless

case class IdxEnv(list: List[Val] = List()) {

  def lookup_val(index: Int): Option[Val] = {
    if (index >= list.length) {
      None
    } else {
      Some(list(index))
    }
  }

  def ext_let(value: Val): IdxEnv = {
    IdxEnv(value +: list)
  }

}
