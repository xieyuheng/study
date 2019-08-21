package xieyuheng.tt.untyped

case class NeutralVar (
  name: String,
) extends Neutral {
  def readBack(_usedNames: Set[String]): Either[ErrorMsg, Exp] =
    Right(Var(name))
}
