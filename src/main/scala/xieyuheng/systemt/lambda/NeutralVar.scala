package xieyuheng.systemt

case class NeuVar (
  name: String,
) extends Neu {
  def readback_neu(usedNames: Set[String]): Either[Err, Exp] = {
    Right(Var(name))
  }
}
