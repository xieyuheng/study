package xieyuheng.untyped

case class NeutralVar (
  name: String,
) extends Neutral {
  def readback(_usedNames: Set[String]): Either[Err, Exp] =
    Right(Var(name))
}
