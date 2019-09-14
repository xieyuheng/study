package xieyuheng.tartlet

case class NeuVar (
  name: String,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    Right(Var(name))
  }
}
