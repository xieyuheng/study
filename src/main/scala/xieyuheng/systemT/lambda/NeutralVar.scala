package xieyuheng.systemT

case class NeutralVar (
  name: String,
) extends Neutral {
  def readbackNeutral(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    Right(Var(name))
  }
}
