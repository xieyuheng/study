package xieyuheng.tt.tartlet

sealed trait Den
final case class Def (t: Value, value: Value) extends Den
final case class Bind (t: Value) extends Den
