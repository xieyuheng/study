package xieyuheng.adventure.untyped

sealed trait Jo
final case class Var(name: String) extends Jo
final case class Let(name: String) extends Jo
final case class JoJo(list: List[Jo]) extends Jo
final case class Define(name: String, jojo: JoJo) extends Jo
final case class Str(str: String) extends Jo
