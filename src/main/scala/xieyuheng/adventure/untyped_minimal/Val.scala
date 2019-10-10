package xieyuheng.adventure.untyped_minimal

sealed trait Val
final case class ValJoJo(list: List[Jo], env: Env) extends Val
