package xieyuheng.adventure.jojo_simple

sealed trait Val
final case class ValJoJo(list: List[Jo], env: Env, ctx: Ctx) extends Val
final case class ValAtom(name: String, str: String) extends Val
