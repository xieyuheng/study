package xieyuheng.adventure.jojo_untyped

sealed trait Jo
final case class Var(name: String) extends Jo
final case class Let(name: String) extends Jo
final case class JoJo(list: List[Jo]) extends Jo
final case class Define(name: String, jojo: JoJo) extends Jo
final case class Str(str: String) extends Jo
final case class Cons() extends Jo
final case class Car() extends Jo
final case class Cdr() extends Jo
