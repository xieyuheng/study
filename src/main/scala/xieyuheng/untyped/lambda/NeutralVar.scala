package xieyuheng.untyped

case class NeuVar (
  name: String,
) extends Neu {
  def readback(_usedNames: Set[String]): Either[Err, Exp] =
    Right(Var(name))
}
