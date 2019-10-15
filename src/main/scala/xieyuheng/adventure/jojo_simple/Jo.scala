package xieyuheng.adventure.jojo_simple

sealed trait Jo
final case class Var(name: String) extends Jo
final case class Let(name: String, t: Ty) extends Jo
final case class JoJo(list: List[Jo]) extends Jo
final case class Claim(name: String, tyty: TyTy) extends Jo
final case class Define(name: String, jojo: JoJo) extends Jo
final case class Execute() extends Jo
final case class AssertEq() extends Jo
final case class ReportDs() extends Jo
final case class ReportRs() extends Jo
final case class Print() extends Jo
final case class Newline() extends Jo
