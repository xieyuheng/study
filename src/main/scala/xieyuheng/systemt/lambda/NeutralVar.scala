package xieyuheng.systemt

case class NeutralVar (
  name: String,
) extends Neutral {
  def readback_neu(usedNames: Set[String]): Either[Err, Exp] = {
    Right(Var(name))
  }
}
