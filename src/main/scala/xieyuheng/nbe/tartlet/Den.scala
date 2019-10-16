package xieyuheng.tartlet

sealed trait Den
final case class Def(t: Val, value: Val) extends Den
final case class Bind(t: Val) extends Den
