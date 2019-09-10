package xieyuheng.untyped

case class NeutralVar (
  name: String,
) extends Neutral {
  def readback(_usedNames: Set[String]): Either[ErrorMsg, Exp] =
    Right(Var(name))
}
