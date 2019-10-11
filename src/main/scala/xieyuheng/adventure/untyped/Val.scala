package xieyuheng.adventure.untyped

sealed trait Val
final case class ValJoJo(list: List[Jo], env: Env) extends Val
final case class ValStr(str: String) extends Val
