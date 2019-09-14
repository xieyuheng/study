package xieyuheng.untyped

case class NeuVar (
  name: String,
) extends Neu {
  def readback(_used_names: Set[String]): Either[Err, Exp] =
    Right(Var(name))
}
